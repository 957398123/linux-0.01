/*
 *  linux/boot/head.s
 *
 *  (C) 1991  Linus Torvalds
 */

/*
 *  head.s contains the 32-bit startup code.
 *
 * NOTE!!! Startup happens at absolute address 0x00000000, which is also where
 * the page directory will exist. The startup code will be overwritten by
 * the page directory.
 */
!syatem模块由boot/head.o init/main.o组成
.text
.globl idt,gdt,pg_dir,tmp_floppy_area
#0x00 页目录
pg_dir:
.globl startup_32
startup_32:
	movl $0x10,%eax /* EAX的低十六位就是AX */
	mov %ax,%ds
	mov %ax,%es
	mov %ax,%fs
	mov %ax,%gs  /* ds, es, fs, gs = 0x10 */
	lss stack_start,%esp  /* Load Pointer Using SS */ 代码表示用 a 的值加载 ESP，用 b 的值(段选择子0x10)加载 SS，即栈的初始化。
	call setup_idt  /* 调用设置idt */
	call setup_gdt  /* 调用设置gdt */
	/* 0x10段选择子，索引号3-15，因为低三位用不了哈 */
	/*这里selctor = 0x02; */
	movl $0x10,%eax		# reload all the segment registers
	/* 这里是在设置gdt表之后，刷新流水线，跟setup.s那里一样的，只不过那里直接jmpi */
	mov %ax,%ds		# after changing gdt. CS was already
	mov %ax,%es		# reloaded in 'setup_gdt'
	mov %ax,%fs
	mov %ax,%gs
	lss stack_start,%esp #重新设置堆栈
	xorl %eax,%eax #eax = 0
	#局部数字标号一般用在例程中循环和条件执行的代码部分，
	#或用在仅在局部使用的小子例程中。
1:	incl %eax		# check that A20 really IS enabled
	movl %eax,0x000000	# loop forever if it isn't
	cmpl %eax,0x100000
	je 1b  #如果相等就是没开启A20,循环直到开启

/*
 * NOTE! 486 should set bit 16, to check for write-protect in supervisor
 * mode. Then it would be unnecessary with the "verify_area()"-calls.
 * 486 users probably want to set the NE (#5) bit also, so as to use
 * int 16 for math errors.
 */
	movl %cr0,%eax		# check math chip
	andl $0x80000011,%eax	# Save PG,PE,ET
/* "orl $0x10020,%eax" here for 486 might be good */
	orl $2,%eax		# set MP
	movl %eax,%cr0
	call check_x87
	jmp after_page_tables
/* 假如是分页，会去一个表里面去找，gdt生成的地址，取高20位乘以4，访问分页表 */
/* 取两个字(32位) + 地址低12位(为什么是12位？因为0xFFF = 4095 = 4KB) */
/* 这样加上这个偏移地址不会到其它的页(页一般4KB) */

/*
 * We depend on ET to be correct. This checks for 287/387.
 */
check_x87:
	fninit
	fstsw %ax
	cmpb $0,%al
	je 1f			/* no coprocessor: have to set bits */
	movl %cr0,%eax
	xorl $6,%eax		/* reset MP, set EM */
	movl %eax,%cr0
	ret
.align 2
1:	.byte 0xDB,0xE4		/* fsetpm for 287, ignored by 387 */
	ret

/*
 *  setup_idt
 *
 *  sets up a idt with 256 entries pointing to
 *  ignore_int, interrupt gates. It then loads
 *  idt. Everything that wants to install itself
 *  in the idt-table may do so themselves. Interrupts
 *  are enabled elsewhere, when we can be relatively
 *  sure everything is ok. This routine will be over-
 *  written by the page tables.
 */
setup_idt:
	lea ignore_int,%edx /* Load Effective Address 用来加载有效地址到寄存器，有效地址，就是偏移地址。  */
	movl $0x00080000,%eax
	/* 这样赋值的话 %eax的值就是 0x0008-ignore_int(offset) */
	/* 注意一下，系统是从0x0000开始的，所以offset是等于实际地址的 */
	movw %dx,%ax		/* selector = 0x0008 = cs */  /*看这个IDT结构 31-16 selector, 15-0 offset, 16-31 offset不用管，反正也是0 */
	/* 这里是access部分 40-47 */
	movw $0x8E00,%dx	/* interrupt gate - dpl=0, present */

	lea idt,%edi  /* edi = idt offset */ 
	mov $256,%ecx /* exc = 256 */
rp_sidt:  /* 循环设置256个idt,注意，eax值是一样的 */
	/* 注意结构，低位在前,为什么这里需要注意？因为这里是64位数据 */
	/* 这里是32位寄存器，如果是64位，直接赋值就可以了。需要注意一下细节(寄存器从内存中取存数据) */
	/* 所以尽量不要用DB DD DW伪指令，放数据很难受 */
	movl %eax,(%edi)
	movl %edx,4(%edi)
	addl $8,%edi /* 注意哈，内存中单位是字节，这里加8，是因为一个idt描述符就是8字节 */
	dec %ecx /* ecx -= 1 */
	jne rp_sidt
	lidt idt_descr  /* 设置idt，返回 */
	ret

/*
 *  setup_gdt
 *
 *  This routines sets up a new gdt and loads it.
 *  Only two entries are currently built, the same
 *  ones that were built in init.s. The routine
 *  is VERY complicated at two whole lines, so this
 *  rather long comment is certainly needed :-).
 *  This routine will beoverwritten by the page tables.
 */
setup_gdt:
	lgdt gdt_descr
	ret

/*
 * I put the kernel page tables right after the page directory(页目录),
 * using 4 of them to span 16 Mb of physical memory. People with
 * more than 16MB will have to expand this(更多就拓展).
 */
.org 0x1000 #伪指令 pg0: == 0x1000
pg0:

.org 0x2000
pg1:

.org 0x3000
pg2:

.org 0x4000
pg3:

.org 0x5000
/*
 * tmp_floppy_area is used by the floppy-driver when DMA cannot
 * reach to a buffer-block. It needs to be aligned, so that it isn't
 * on a 64kB border.
 */
tmp_floppy_area: #软盘缓冲区1KB
	.fill 1024,1,0

#检查x87协处理器之后跳转到这里
after_page_tables:
	pushl $0		# These are the parameters to main :-)
	pushl $0
	pushl $0
	pushl $L6		# return address for main, if it decides to.
	pushl $main     #main方法地址入栈 jmp那里ret,就会进到main了
	jmp setup_paging
L6:
	jmp L6			# main should never return here, but
				# just in case, we know what happens.

/* This is the default interrupt "handler" :-) */
int_msg:
	.asciz "Unknown interrupt\n\r"
.align 2
ignore_int:
	pushl %eax
	pushl %ecx
	pushl %edx
	push %ds
	push %es
	push %fs
	movl $0x10,%eax
	mov %ax,%ds
	mov %ax,%es
	mov %ax,%fs
	pushl $int_msg
	call printk
	popl %eax
	pop %fs
	pop %es
	pop %ds
	popl %edx
	popl %ecx
	popl %eax
	iret


/*
 * Setup_paging
 *
 * This routine sets up paging by setting the page bit
 * in cr0. The page tables are set up, identity-mapping
 * the first 16MB. The pager assumes that no illegal
 * addresses are produced (ie >4Mb on a 4Mb machine).
 *
 * NOTE! Although all physical memory should be identity
 * mapped by this routine, only the kernel page functions
 * use the >1Mb addresses directly. All "normal" functions
 * use just the lower 1Mb, or the local data space, which
 * will be mapped to some other place - mm keeps track of
 * that.
 *
 * For those with more memory than 16 Mb - tough luck. I've
 * not got it, why should you :-) The source is here. Change
 * it. (Seriously - it shouldn't be too difficult. Mostly
 * change some constants etc. I left it at 16Mb, as my machine
 * even cannot be extended past that (ok, but it was cheap :-)
 * I've tried to show which constants to change by having
 * some kind of marker at them (search for "16Mb"), but I
 * won't guarantee that's all :-( )
 */
.align 2
setup_paging:
    /* 页目录加4个页表 */
	#每个页表占用1024个双字（双字=4B），共5个页表
	movl $1024*5,%ecx		/* 5 pages - pg_dir+4 page tables */
	xorl %eax,%eax
	xorl %edi,%edi			/* pg_dir is at 0x000 */
	#EAX, EDI = 0
	/* CLD指令复位方向标志：DF=0，STD指令置位方向标志：DF=1 */
	/* 方向标志DF用来决定在串操作指令执行时有关指针寄存器发生调整的方向。 */
	/* https://blog.csdn.net/better0332/article/details/2293884 */
	/* 串操作指令共有五种（MOVS、STOS、LODS、CMPS、SCAS） */
	cld;
	# REP 根据cx的值，重复执行后面的指令
	# 这里ES = 0x10
	rep;stosl # eax -> es:[edi],edi每次增加4，重复ecx次
	#这个页表地址为什么要加7?
	#页目录项和页表项
	#https://blog.csdn.net/longintchar/article/details/52166130
	movl $pg0+7,pg_dir		/* set present bit/user r/w */
	movl $pg1+7,pg_dir+4		/*  --------- " " --------- */
	movl $pg2+7,pg_dir+8		/*  --------- " " --------- */
	movl $pg3+7,pg_dir+12		/*  --------- " " --------- */
	movl $pg3+4092,%edi
	movl $0xfff007,%eax		/*  16Mb - 4096 + 7 (r/w user,p) */
	std
	#STOSL指令相当于将EAX中的值保存到ES:EDI指向的地址中，
	#若设置了EFLAGS中的方向位置位(即在STOSL指令前使用STD指令)则EDI自减4，
	#否则(使用CLD指令)EDI自增4。
1:	stosl			/* fill pages backwards - more efficient :-) */
	subl $0x1000,%eax #填充页表，不过是倒着填的。0x1000 = 4KB
	jge 1b
	#这样的层次化分页结构是每个任务都有的，
	#或者说每个任务都有自己的页目录。
	#在处理器内部，有一个控制寄存器叫 CR3，
	#存放着当前任务的页目录的物理地址，
	#故 CR3 又叫做页目录基址寄存器（Page Directory Base Register，PDBR）.
	xorl %eax,%eax		/* pg_dir is at 0x0000 */
	movl %eax,%cr3		/* cr3 - page directory start */
	movl %cr0,%eax
	orl $0x80000000,%eax
	movl %eax,%cr0		/* set paging (PG) bit, 开启分页 */ 
	ret			/* this also flushes prefetch-queue */

.align 2
.word 0
/* https://www.felixcloutier.com/x86/lgdt:lidt */
idt_descr:
	.word 256*8-1		# limit idt contains 256 entries 直接写0x0800也可以的
	.long idt #base address (a linear address) 啊哈，这里确实是线性的。系统模块是0x0000开始的，而且开头就是head.s
.align 2
.word 0
gdt_descr:
	.word 256*8-1		# so does gdt (not that that's any
	.long gdt		# magic number, but it works for me :^)

	.align 8
!.fill repeat , size , value /* .fill伪指令的作用是反复拷贝size个字节，重复repeat次。 */
idt:	.fill 256,8,0		# idt is uninitialized

#这里都是分页的。4KB
gdt:	.quad 0x0000000000000000	/* NULL descriptor */
	/* granularity=4096, 386 */
	/* baseAddress = 0x00000000 limt = 0X0FFF = 4095 (4096 * 4096) = 16MB*/
	.quad 0x00c09a0000000fff	/* 16Mb */ /* 内存中由低到高是 FF-0F-00-00-00-9A-C0-00 */
	/* baseAddress = 0x00000000 */
	.quad 0x00c0920000000fff	/* 16Mb */
	/* baseAddress = 0x00000000 */
	.quad 0x0000000000000000	/* TEMPORARY - don't use */
	.fill 252,8,0			/* space for LDT's and TSS's etc */
