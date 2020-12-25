!
!	setup.s		(C) 1991 Linus Torvalds
!
! setup.s is responsible(负责) for getting the system data from the BIOS,
! and putting them into the appropriate(合适) places in system memory.
! both setup.s and system has been loaded by the bootblock(引导块bootsect).
!
! This code asks the bios for memory/disk/other parameters, and
! puts them in a "safe" place: 0x90000-0x901FF, ie where the
! boot-block used to be. It is then up to the protected mode
! system to read them from there before the area is overwritten
! for buffer-blocks.
!

! NOTE! These had better be the same as in bootsect.s!

INITSEG  = 0x9000	! we move boot here - out of the way
SYSSEG   = 0x1000	! system loaded at 0x10000 (65536).
SETUPSEG = 0x9020	! this is the current segment

.globl begtext, begdata, begbss, endtext, enddata, endbss
.text
begtext:
.data
begdata:
.bss
begbss:
.text

entry start
start:

! ok, the read went well so we get current cursor position and save it for
! posterity.
! GET CURSOR POSITION AND SIZE
	mov	ax,#INITSEG	! this is done in bootsect already, but...
	mov	ds,ax
	mov	ah,#0x03	! read cursor pos
	xor	bh,bh
	int	0x10		! save it in known place, con_init fetches
	mov	[0],dx		! it from 0x90000. 默认DS寄存器 这里DS = 0x9000
! Get memory size (extended mem, kB)
	mov	ah,#0x88
	int	0x15
	mov	[2],ax

! Get video-card data:

	mov	ah,#0x0f
	int	0x10
	mov	[4],bx		! bh = display page
	mov	[6],ax		! al = video mode, ah = window width

! check for EGA/VGA and some config parameters

	mov	ah,#0x12
	mov	bl,#0x10
	int	0x10
	mov	[8],ax
	mov	[10],bx
	mov	[12],cx

! Get hd0 data

	mov	ax,#0x0000
	mov	ds,ax
	lds	si,[4*0x41]
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0080
	mov	cx,#0x10
	rep
	movsb !rep指令常和串传送指令搭配使用功能：根据cx的值，重复执行后面的指令

! Get hd1 data

	mov	ax,#0x0000
	mov	ds,ax
	lds	si,[4*0x46]
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0090
	mov	cx,#0x10
	rep
	movsb

! Check that there IS a hd1 :-)

	mov	ax,#0x01500
	mov	dl,#0x81
	int	0x13
	jc	no_disk1
	cmp	ah,#3
	je	is_disk1
no_disk1:
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0090
	mov	cx,#0x10
	mov	ax,#0x00
	rep
	stosb
is_disk1:
!从实模式切换到保护模式
! now we want to move to protected mode ...

	cli			! no interrupts allowed !不允许中断

! first we move the system to it's rightful place 将system移动到正确的位置

	mov	ax,#0x0000
	cld			! 'direction'=0, movs moves forward
!　cld相对应的指令是std，二者均是用来操作方向标志位DF（Direction Flag）。
！cld使,DF 复位，即是让DF=0，std使DF置位，即DF=1.这两个指令用于串操作指令中。
！通过执行cld或std指令可以控制方向标志DF，
！决定内存地址是增大（DF=0，向高地址增加）
！还是减小（DF=1，向地地址减小）。
do_move:
	mov	es,ax		! destination segment
	add	ax,#0x1000
	cmp	ax,#0x9000  ！if ax = 0x9000 jmp end_move
	jz	end_move
	mov	ds,ax		! source segment
	sub	di,di       !di = 0
	sub	si,si       !si = 0
	mov 	cx,#0x8000    !移动0x10000-0x80000的内容到0x00000,直到CX = 0,大小为512KB,bootsect也说了,它内核在512KB内够了。
	rep
	movsw !移动数据
	jmp	do_move

! then we load the segment descriptors

end_move:
	mov	ax,#SETUPSEG	! right, forgot this at first. didn't work :-)
	mov	ds,ax
	lidt	idt_48		! load idt with 0,0 设置中断表
	lgdt	gdt_48		! load gdt with whatever appropriate 设置全局表

!当ibm pc at系统被制造出来时，新的286处理器以及以后版本和旧有x86处理器并不兼容。
!旧的X86处理器（Intel 8086）有20位地址总线，这样可以访问最高1M内存。
!而386和以后版本有32地址总线，这样可以访问最高4G的内存。
!但是旧的8086处理器没有这么大的地址总线。
!为了保持兼容性Intel在地址线的第20位上制造了一个逻辑OR门，以便可以开启或关闭超过20位的地址总线。
!这样，为了兼容旧的处理器，在机器开启时A20被禁止的。
!A20线是一个OR逻辑电路门，被放置在第20位的地址总线上，而且可以开启或关闭。
!这是通过键盘控制器的P21线来完成的，这样，通过键盘控制器可以开启开关闭A20线。
!开启A20线有三种方法。或者你可以通过使用象HIMEM.sys或者象GRUB这样的bootloaders来跳过这些步骤（GRUB会设置你的机器开启A20线并进入保护模式）。
!1. Keyboard Controller
!这是开启A20线通常的作法。键盘的微控制器提供了一个关闭或开启A20线的功能。
!在开启A20线之前需要关闭中断以防止我们的内核陷入混乱。命令字节是通过端口0x64来发送的。
!2. BIOS Function
!The INT 15 2400,2401,2402 are used to disable,enable,return status of the A20 Gate respectively.
!3. System Port
! that was painless, now we enable A20

	call	empty_8042
	mov	al,#0xD1		! command write
	out	#0x64,al
	call	empty_8042
	mov	al,#0xDF		! A20 on
	out	#0x60,al
	call	empty_8042

! well, that went ok, I hope. Now we have to reprogram the interrupts :-(
! we put them right after the intel-reserved hardware interrupts, at
! int 0x20-0x2F. There they won't mess up anything. Sadly IBM really
! messed this up with the original PC, and they haven't been able to
! rectify it afterwards. Thus the bios puts interrupts at 0x08-0x0f,
! which is used for the internal hardware interrupts as well. We just
! have to reprogram the 8259's, and it isn't fun.
！初始化PIC(可编程中断控制器)
!PIC是将8个中断信号集合成一个中断信号的装置
！与CPU直接相连的PIC称为主PIC，与主PIC相连的是从PIC。
！主PIC负责0-7号中断信息，从PIC负责8-15号中断信息
！此外，从PIC通过第二号IRQ与主PIC相连，主板上的配线就是这样，无法用软件来改变。
！从CPU角度来看，PIC是外部设备，CPU使用OUT指令进行操作。
!PIC0 主PIC， PIC1 从PIC， PIC内部有很多寄存器，用端口号码对彼此进行区别。
!PIC有些细微规则，比如写入ICW1后，紧跟着一定要写入ICW2等。
！PIC寄存器都是8位寄存器。IMR是interrupt mask register的缩写，意思是中断屏蔽器。
！8位分别对应8路IRQ信号，如果某一位的值是1，则该位所对应的IRQ信号被屏蔽，PIC就忽视该路信号。
！这主要是因为，正在对中断设定进行更改时，如果再接受别的中断会引起混乱，为了防止这种事情发生，就必须屏蔽中断。
！还有，如果某个IRQ没有连接任何设备的话，静电干扰也可能会引起反应，导致操作系统混乱，所以也要屏蔽这类干扰。
！ICW是initial control word的缩写，意为初始化控制数据。
！ICW有4个，分别编号1-4，共有4个字节的数据。
!ICW 1, 4和PIC主板配线方式、中断信号的电气特性等有关，所以不详细说明。
！ICW3是有关主-从连接的设定，对于主PIC而言，第几号IRQ与从PIC相连是用8位来设定的。如果全为1，那么主PIC就能驱动8个从PIC(那样的话，最大可以有个IRQ)
！对于从PIC来说，该从PIC与主PIC的第几号相连，用3位来设定。因为硬件上已经不能更改了，如果软件上设定不一致，会发生错误。
！ICW2决定以哪一号中断通知CPU
！中断发生后，如果CPU可以处理这个中断，CPU就会命令PIC发送两个字节的数据。
！这两个字节的数据是怎么传送的呢？CPU与PIC用IN或OUT进行数据传送时，有数据信号线连在一起。
！PIC就是利用这个信号线发送这两个字节数据的。送过来的数据是 0xcd 0x??这两个字节。
！由于电路设计的原因，这两个字节的数据在CPU看来，与从内存都进来的程序是完全一样的，所以CPU就把送过来的0xcd 0x??作为机器语言执行。
！这恰恰就是把数据当作程序来执行的情况。这里的0xcd就是调用BIOS时的INT指令。
！前面0x20-0x2F是因为INT 0X00-0X1F不能用于IRQ
！这是因为，应用程序想要对操作系统干坏事的时候，CPU内部会自动产生INT 0X00-0X1F
！如果IRQ与这些号码重复了，CPU就分不清它到底是IRQ还是CPU的系统保护通知。
！如果IRQ与这些,
!#define PIC0_ICW1		0x0020
!#define PIC0_OCW2		0x0020
!#define PIC0_IMR		0x0021
!#define PIC0_ICW2		0x0021
!#define PIC0_ICW3		0x0021
!#define PIC0_ICW4		0x0021
!#define PIC1_ICW1		0x00a0
!#define PIC1_OCW2		0x00a0
!#define PIC1_IMR		0x00a1
!#define PIC1_ICW2		0x00a1
!#define PIC1_ICW3		0x00a1
!#define PIC1_ICW4		0x00a1
!在80X86微机机系统中采用了8259A可编程中断控制器芯片。
!每个8259A芯片可以管理8个中断源。通过多片级联方式，
!8259A能构成最多管理64个中断向量的系统。
!0x00eb,0x00eb这两条指令共可提供 14~20 个 CPU 时钟周期的延迟时间。
!在 as86 中没有表示相应指令的助记符，因此 Linus 在 setup.s 等一些汇编程序中就直接使用机器码来表示这种指令。
!另外，每个空操作指令 N0P 的时钟周期数是 3 个，
!因此若要达到相同的延迟效果就需要 6 至 7 个 N0P 指令。
!https://blog.csdn.net/longintchar/article/details/79464007
！https://blog.csdn.net/longintchar/article/details/79439466
	!PIC_ICW1 当发送的字节第 5 比特位(D4)=1，并且地址线 A0=0 时，表示是对 ICW1 编程。
	!D4 1 恒为1
	!D3 LTIM 1-电平触发中断; 0-边沿触发模式
	!D2 ADI
	!D1 SNGL 1-单片8259A; 0 多片
	!D0 IC4 1-需要ICW4; 0-不需要
	mov	al,#0x11		! initialization sequence 0x11 = 0x00010001
	out	#0x20,al		! send it to 8259A-1
	.word	0x00eb,0x00eb		! jmp $+2, jmp $+2 !端口操作中的这类指令的作用应该就是延时。$是当前指令地址
	out	#0xA0,al		! and to 8259A-2
	.word	0x00eb,0x00eb
	!PIC_ICW2 设置主PIC中断开始于0x20
	mov	al,#0x20		! start of hardware int's (0x20)
	out	#0x21,al
	.word	0x00eb,0x00eb
	!PIC_ICW2 设置从PIC中断开始于0x28
	mov	al,#0x28		! start of hardware int's 2 (0x28)
	out	#0xA1,al
	.word	0x00eb,0x00eb
	!设置ICW3
	mov	al,#0x04		! 8259-1 is master 主PIC
	out	#0x21,al
	.word	0x00eb,0x00eb
	mov	al,#0x02		! 8259-2 is slave 从PIC
	out	#0xA1,al
	.word	0x00eb,0x00eb
	!设置ICW4
	mov	al,#0x01		! 8086 mode for both
	out	#0x21,al
	.word	0x00eb,0x00eb
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0xFF		! mask off all interrupts for now 屏蔽主PIC和从PIC的中断请求
	out	#0x21,al
	.word	0x00eb,0x00eb
	out	#0xA1,al

!注：30天操作系统那里是直接设置CR0这个控制寄存器
！; プロテクトモード移行
!
!		LGDT	[GDTR0]			; 暫定GDTを設定
！		MOV		EAX,CR0
！		AND		EAX,0x7fffffff	; bit31を0にする（ページング禁止のため）
！		OR		EAX,0x00000001	; bit0を1にする（プロテクトモード移行のため）
！		MOV		CR0,EAX
！		JMP		pipelineflush
！pipelineflush:
！		MOV		AX,1*8			;  読み書き可能セグメント32bit
！		MOV		DS,AX
！		MOV		ES,AX
!		MOV		FS,AX
!		MOV		GS,AX
!		MOV		SS,AX
!
!; bootpackの転送
！
！		MOV		ESI,bootpack	; 転送元
！		MOV		EDI,BOTPAK		; 転送先
！		MOV		ECX,512*1024/4
！		CALL	memcpy
! well, that certainly wasn't fun :-(. Hopefully it works, and we don't
! need no steenking BIOS anyway (except for the initial loading :-).
! The BIOS-routine wants lots of unnecessary data, and it's less
! "interesting" anyway. This is how REAL programmers do it.
!
! Well, now's the time to actually move into protected mode. To make
! things as simple as possible, we do no register set-up or anything,
! we let the gnu-compiled 32-bit programs do that. We just jump to
! absolute address 0x00000, in 32-bit protected mode.
！首先加载机器状态字（lmsw，Load Machine Status Word），
！也称控制寄存器 CR0，其比特位 0 置 1 将使 CPU 切换到保护模式，并且运行在特权级0，
！即当前特权级 CPL = 0。
！此时各个段寄存器仍然指向与实地址模式中相同的线性地址处（在实地址模式下线性地址与物理地址相同）。
！在设置该比特位后，随后一条指令必须是一条段间跳转指令，用于刷新CPU当前指令队列。
！因为 CPU 是在执行一条指令之前就已从内存读取该指令并对其进行译码。
！然而在进入保护模式以后那些属于实模式的预先取得的指令信息就变得不再有效。
！而一条段间跳转指令就会刷新 CPU 的当前指令队列，即丢弃这些无效信息。
！另外，Intel手册上建议 80386 或以上 CPU 应该使用指令 mov cr0,ax 切换到保护模式。
！lmsw 指令仅用于兼容以前的 286 CPU。
!jmpi 0,8 中的 “8 ”是保护模式下的段选择子，
!用于选择描述符表（GDT或LDT）和描述符表项以及所要求的特权级。
!段选择子长度为16位（2字节）。
!b1-b0	请求特权级（RPL）
!b2	0:全局描述符表 1:局部描述符表
!b15-b3	描述符表项的索引, 指出选择第几项描述符(从0开始)
!8 = 0000-0000-0000-1000 ->b15-b0 b3=1
	mov	ax,#0x0001	! protected mode (PE) bit
	lmsw	ax		! This is it! Load Machine Status Word
	jmpi	0,8		! jmp offset 0 of segment 8 (cs) jmpi 0,8 段间跳转指令。执行后，CS=8，IP=0.
!即使是在实模式下，段寄存器的描述符高速缓存器也被用于访问内存，仅低20位有效，
!高12位是全零。当处理器进入保护模式后，这些内容依然残留着，但不影响使用，
!程序可以继续执行。但是，这些残留的内容在保护模式下是无效的，
!迟早会在执 行某些指令的时候出问题。
!因此，比较安全的做法是尽快刷新 CS、SS、DS 、ES 、FS 和 GS 的内容，
! 包括它们的段选择器和描述符高速缓存器
！处理器遇见跳转指令会清空流水线,并串行化执行。
! 段间跳转会重新加载段选择器CS，并刷新描述符高速缓存器中的内容。
! This routine checks that the keyboard command queue is empty
! No timeout is used - if this hangs there is something wrong with
! the machine, and we probably couldn't proceed anyway.
empty_8042:
	.word	0x00eb,0x00eb
	in	al,#0x64	! 8042 status port
	test	al,#2		! is input buffer full?
	jnz	empty_8042	! yes - loop
	ret

！全局描述表用于内存地址的转换。
！所有程序的内存访问都需要用到GDT中的有关内存区域即x86内存分段的信息。
！访问GDT需要使用segment selector和segment offset。
！处理器使用segment selector为索引查找GDT的条目。
！当适当的条目找到后，处理器将会做一系列的检查，
！包括检查segment offset尝试访问区间是否在此内存分段内，
！代码是否有权限访问此内存分段(检查分级保护域权限）等。
!gdt结构
!struct SEGMENT_DESCRIPTOR {
!	short limit_low(0-15), base_low(0-15);
!	char base_mid(16-23), access_right;
!	char limit_high, base_high(24-31);
!};
!段的生成函数
!void set_segmdesc(struct SEGMENT_DESCRIPTOR *sd, unsigned int limit, int base, int ar)
!	{
!		if (limit > 0xfffff) {
!		ar |= 0x8000; /* G_bit = 1 */
!		limit /= 0x1000;
!		}
!		sd->limit_low = limit & 0xffff;
!		sd->base_low = base & 0xffff;
!		sd->base_mid = (base >> 16) & 0xff;
!		sd->access_right = ar & 0xff;
!		sd->limit_high = ((limit >> 16) & 0x0f) | ((ar >> 8) & 0xf0);
!		sd->base_high = (base >> 24) & 0xff;
!		return;
!	}
gdt:
	.word	0,0,0,0		! dummy

!base = base_low=0x0000, base_mid = 00, base_high = 00
!The limit, a 20 bit value, tells the maximum addressable unit (either in 1 byte units, or in pages).
	.word	0x07FF		! 8Mb - limit=2047 (2048*4096=8Mb)
	.word	0x0000		! base address=0
	.word	0x9A00		! code read/exec
	.word	0x00C0		! granularity=4096, 386

	.word	0x07FF		! 8Mb - limit=2047 (2048*4096=8Mb)
	.word	0x0000		! base address=0
	.word	0x9200		! data read/write
	.word	0x00C0		! granularity=4096, 386

idt_48:
	.word	0			! idt limit=0
	.word	0,0			! idt base=0L

!因为段寄存器是16位,但由于CPU设计上的原因,段寄存器的低3位不能用.因此只能用13位来表示段号.
!2^(10+3)=8*1024=8192,则能够处理段号为0~8191
!gdtr 48位寄存器
!前16位
!段上限,它等于"GDT的有效字节数-1".
!偶尔会用到上限这个词,意思都是表示量的大小,
!一般为"字节数-1"
!后32位
!代表GDT的开始地址
!GDTR的值不能用MOV指令来赋值.给它赋值的时候,唯一的办法就是指定一个内存的地址。
!从指定的内存地址读取6个字节,然后赋值给GDTR寄存器.
!完成这一任务的指令,就是LGDT.
gdt_48:
	.word	0x800		! gdt limit=2048, 256 GDT entries 二进制= 00 08
	!这里注意一下有几个要点
	!汇编语言指定内存地址时，低位在前,高位在后。这里这个word是伪指令
	!还有，setup.s地址是在0x90200，所以gdt(相对地址)那里要加上512(注：这里是10进制)
	!所以这里的地址会转换成0x0009xxxx xxxx = gdt + 512
	.word	512+gdt,0x9	! gdt base = 0X9xxxx 二进制(这里是16进制表示)=14 03 09 00
	
.text
endtext:
.data
enddata:
.bss
endbss:
