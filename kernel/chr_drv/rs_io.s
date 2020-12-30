/*
 *  linux/kernel/rs_io.s
 *
 *  (C) 1991  Linus Torvalds
 */

/*
 *	rs_io.s
 * ���շ�����(�ֽ�)��ɺ�ᴥ���ж�
 * ���ļ�ʵ��rs232 ����ͨ���жϴ���
 * This module implements the rs232 io interrupts.
 */

.text
.globl rs1_interrupt,rs2_interrupt

/* ��д���л��������� */
size	= 1024				/* must be power of two !
					   and must match the value
					   in tty_io.c!!! */

/* these are the offsets into the read/write buffer structures */
/* tty_queue�ṹ��λ��ƫ���� */
rs_addr = 0
head = 4
tail = 8
proc_list = 12
buf = 16

startup	= 256		/* chars left in write queue when we restart it */

/*
 * table_list,������tty_io.c��
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
	pop %es            /* ����ds, es = 0x10,0x10�Ƕ�ѡ���ӡ� */
	movl 24(%esp),%edx /* ��table_list(tty_queues����)��ַ��ֵ��edx */
	movl (%edx),%edx   /* ���ڴ���ȡֵ,ȡ��read_q(tty_queue)��ַ����ֵ��edx */
	movl rs_addr(%edx),%edx /* ��tty_queue�ṹ���е�data(�˿�ֵ)��ֵ��edx */
	addl $2,%edx		/* interrupt ident. reg, edx��ǰֵΪ�жϱ�ʶ�Ĵ����˿�IIR */
rep_int:
	xorl %eax,%eax    /* eax = 0 */
	inb %dx,%al       /* ͨ����ȡIIR��ֵ����ȷ���ж�����˭������ */
	testb $1,%al      /* ��Bit0 = 1ʱ,���ж�,Bit0 = 0,�ж���δ����,���������жϾ���ת��end */
	jne end           /* TEMP ��SRC1 AND SRC2; SF ��MSB(TEMP); IF TEMP = 0 THEN ZF ��1; ELSE ZF ��0; */
	cmpb $6,%al		/* this not should happen, but ... */ /* ���ﲻ��������ΪIIR��Bit3~Bit7��Ϊ0,���Բ�����ڴ���6������� */
	ja end          /* ���IIR��ֵ����6,��ת��end */
	movl 24(%esp),%ecx          /* ����ecx��ֵΪtty_queuesָ�� */
	pushl %edx
	subl $2,%edx                /* edx�˿�ֵ=tty_table[].read_q.data */
	/* 
	*  �ܽ�������,alֻ�����¼���ֵ
	*  0x0000 = 0d(�ж������ڵ��ƽ����״̬)
	*  0x0010 = 2d(����������Ĵ�������)
	*  0x0100 = 4d(������ϢΪ��Ч)
	*  0x0110 = 6d(���ӿ���״̬)
	*/
	call jmp_table(,%eax,2)		/* NOTE! not *4, bit0 is 0 already, address = cs : [eax * 2 + jmp_table], ע��һ�¹�����������Ǵ�0x0000��ʼ�ģ������Եġ� */
	popl %edx                   /* ��λedx��ֵΪ�˿ں� */
	jmp rep_int
end:	movb $0x20,%al   /* al = 0x20 */
	outb %al,$0x20		/* EOI */ /* ���жϴ���֮����8259A����EOI��֪ͨ���жϴ�������� */
	pop %ds
	pop %es
	popl %eax
	popl %ebx
	popl %ecx
	popl %edx
	/* �������Ҫ,��Ȼ���������ˡ� */
	addl $4,%esp		# jump over _table_list entry, �����������(tty_table)ָ�롣����ǰ��push $table_list+8 or $table_list+ 16
	iret                /* IERT������RET֮һ���ǻ��ᵯ��FLAGE��ֵ */

/*ע��ִ����δ���ʱ�Ĵ�����ֵ*/
/*
*%ecx table_list(tty_queues����)��ַ
*%edx �˿�ֵ,0X2F8����0x3F8
*/
jmp_table:
	.long modem_status,write_char,read_char,line_status /* .long expressions: ����һ��������, ��Ϊ֮����ռ�. */
.align 2
/* ���������ڵ��ƽ����״̬���ж� */
modem_status:
	addl $6,%edx		/* clear intr by reading modem status reg, edx=0x2FE����0x3FE */
	inb %dx,%al         /* ͨ������·״̬�Ĵ������и�λ */
	ret

/* �����������ӿ���״̬���ж� */
.align 2
line_status:
	addl $5,%edx		/* clear intr by reading line status reg. */
	inb %dx,%al
	ret

/* ���������ڽ�����ϢΪ��Ч���ж� */
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

/* ���������ڴ���������Ĵ������ŵ��ж� */
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
