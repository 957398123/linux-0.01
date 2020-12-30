/*
 *  linux/kernel/rs_io.s
 *
 *  (C) 1991  Linus Torvalds
 */

/*
 *	rs_io.s
 * 在收发数据(字节)完成后会触发中断
 * 该文件实现rs232 串行通信中断处理
 * This module implements the rs232 io interrupts.
 */

.text
.globl rs1_interrupt,rs2_interrupt

/* 读写队列缓冲区长度 */
size	= 1024				/* must be power of two !
					   and must match the value
					   in tty_io.c!!! */

/* these are the offsets into the read/write buffer structures */
/* tty_queue结构体位置偏移量 */
rs_addr = 0
head = 4
tail = 8
proc_list = 12
buf = 16

startup	= 256		/* chars left in write queue when we restart it */

/*
 * table_list,定义在tty_io.c中
 * These are the actual interrupt routines. They look where
 * the interrupt is coming from, and take appropriate action.
 */
.align 2
rs1_interrupt:
	pushl $table_list+8
	jmp rs_int
.align 2
rs2_interrupt:
	pushl $table_list+16
rs_int:
	pushl %edx
	pushl %ecx
	pushl %ebx
	pushl %eax
	push %es
	push %ds		/* as this is an interrupt, we cannot */
	pushl $0x10		/* know that bs is ok. Load it */
	pop %ds
	pushl $0x10
	pop %es            /* 设置ds, es = 0x10,0x10是段选择子。 */
	movl 24(%esp),%edx /* 将table_list(tty_queues索引)地址赋值给edx */
	movl (%edx),%edx   /* 从内存中取值,取出read_q(tty_queue)地址并赋值给edx */
	movl rs_addr(%edx),%edx /* 将tty_queue结构体中的data(端口值)赋值给edx */
	addl $2,%edx		/* interrupt ident. reg, edx当前值为中断辨识寄存器端口IIR */
rep_int:
	xorl %eax,%eax    /* eax = 0 */
	inb %dx,%al       /* 通过读取IIR的值可以确定中断是由谁引发的 */
	testb $1,%al      /* 当Bit0 = 1时,无中断,Bit0 = 0,中断尚未处理,这里是无中断就跳转到end */
	jne end           /* TEMP ←SRC1 AND SRC2; SF ←MSB(TEMP); IF TEMP = 0 THEN ZF ←1; ELSE ZF ←0; */
	cmpb $6,%al		/* this not should happen, but ... */ /* 这里不可能是因为IIR的Bit3~Bit7恒为0,所以不会存在大于6的情况。 */
	ja end          /* 如果IIR的值大于6,跳转到end */
	movl 24(%esp),%ecx          /* 设置ecx的值为tty_queues指针 */
	pushl %edx
	subl $2,%edx                /* edx端口值=tty_table[].read_q.data */
	/* 
	*  能进到这里,al只有以下几个值
	*  0x0000 = 0d(中断来自于调制解调器状态)
	*  0x0010 = 2d(传送器保存寄存器空着)
	*  0x0100 = 4d(接受信息为有效)
	*  0x0110 = 6d(连接控制状态)
	*/
	call jmp_table(,%eax,2)		/* NOTE! not *4, bit0 is 0 already, address = cs : [eax * 2 + jmp_table], 注意一下哈，这个代码是从0x0000开始的，是线性的。 */
	popl %edx                   /* 复位edx的值为端口号 */
	jmp rep_int
end:	movb $0x20,%al   /* al = 0x20 */
	outb %al,$0x20		/* EOI */ /* 在中断处理之后向8259A发送EOI，通知它中断处理结束。 */
	pop %ds
	pop %es
	popl %eax
	popl %ebx
	popl %ecx
	popl %edx
	/* 这个很重要,不然程序乱套了。 */
	addl $4,%esp		# jump over _table_list entry, 丢弃缓冲队列(tty_table)指针。就是前面push $table_list+8 or $table_list+ 16
	iret                /* IERT区别于RET之一就是还会弹出FLAGE的值 */

/*注意执行这段代码时寄存器的值*/
/*
*%ecx table_list(tty_queues索引)地址
*%edx 端口值,0X2F8或者0x3F8
*/
jmp_table:
	.long modem_status,write_char,read_char,line_status /* .long expressions: 定义一个长整型, 并为之分配空间. */
.align 2
/* 处理来自于调制解调器状态的中断 */
modem_status:
	addl $6,%edx		/* clear intr by reading modem status reg, edx=0x2FE或者0x3FE */
	inb %dx,%al         /* 通过读线路状态寄存器进行复位 */
	ret

/* 处理来自连接控制状态的中断 */
.align 2
line_status:
	addl $5,%edx		/* clear intr by reading line status reg. */
	inb %dx,%al
	ret

/* 处理来自于接受信息为有效的中断 */
.align 2
read_char:
	inb %dx,%al
	movl %ecx,%edx
	subl $table_list,%edx
	shrl $3,%edx
	movl (%ecx),%ecx		# read-queue
	movl head(%ecx),%ebx
	movb %al,buf(%ecx,%ebx)
	incl %ebx
	andl $size-1,%ebx
	cmpl tail(%ecx),%ebx
	je 1f
	movl %ebx,head(%ecx)
1:	pushl %edx
	call do_tty_interrupt
	addl $4,%esp
	ret

/* 处理来自于传送器保存寄存器空着的中断 */
.align 2
write_char:
	movl 4(%ecx),%ecx		# write-queue
	movl head(%ecx),%ebx
	subl tail(%ecx),%ebx
	andl $size-1,%ebx		# nr chars in queue
	je write_buffer_empty
	cmpl $startup,%ebx
	ja 1f
	movl proc_list(%ecx),%ebx	# wake up sleeping process
	testl %ebx,%ebx			# is there any?
	je 1f
	movl $0,(%ebx)
1:	movl tail(%ecx),%ebx
	movb buf(%ecx,%ebx),%al
	outb %al,%dx
	incl %ebx
	andl $size-1,%ebx
	movl %ebx,tail(%ecx)
	cmpl head(%ecx),%ebx
	je write_buffer_empty
	ret
.align 2
write_buffer_empty:
	movl proc_list(%ecx),%ebx	# wake up sleeping process
	testl %ebx,%ebx			# is there any?
	je 1f
	movl $0,(%ebx)
1:	incl %edx
	inb %dx,%al
	jmp 1f
1:	jmp 1f
1:	andb $0xd,%al		/* disable transmit interrupt */
	outb %al,%dx
	ret
