!
! SYS_SIZE is the number of clicks (16 bytes) to be loaded.
! 0x3000 is 0x30000 bytes = 196kB, more than enough for current
! versions of linux
!没剥壳之前system是344.9kB,strip后118.7kB,远小于196kB,还有很充裕的空间
!
!=伪指令 SYSSIZE为0x3000
SYSSIZE = 0x3000
!
!	bootsect.s		(C) 1991 Linus Torvalds
!
! bootsect.s is loaded at 0x7c00 by the bios-startup routines, and moves
! iself out of the way to address 0x90000, and jumps there.
!
! It then loads 'setup' directly after itself (0x90200), and the system
! at 0x10000, using BIOS interrupts. 
!
! NOTE! currently system is at most 8*65536 bytes long. This should be no
! problem, even in the future. I want to keep it simple. This 512 kB
! kernel size should be enough, especially as this doesn't contain the
! buffer cache as in minix
!
! The loader has been made as simple as possible, and continuos
! read errors will result in a unbreakable loop. Reboot by hand. It
! loads pretty fast by getting whole sectors at a time whenever possible.

!注意这是AS86汇编
!.global关键字用来让一个符号对链接器可见，可以供其他链接对象模块使用。
.globl begtext, begdata, begbss, endtext, enddata, endbss
!.text, .data, .bss 分别定义当前代码段、数据段和未初始化数据段。
!在链接 多个目标模块时，链接程序 ld86 会根据它们的类别把各个目标模块中的相应段
!分别组合在一起。这里把三个段都定义在同一重叠地址范围中，因此本程序实际上不分段。
.text定义一个代码段，处理器开始执行代码的地方，代表后面是代码，这是GCC必须的；
.text
begtext:
!数据区
.data
begdata:
!BSS段（bsssegment）通常是指用来存放程序中未初始化的全局变量的一块内存区域。
!BSS是英文BlockStarted by Symbol的简称。BSS段属于静态内存分配。
.bss
begbss:
.text

SETUPLEN = 4				! nr of setup-sectors
BOOTSEG  = 0x07c0			! original address of boot-sector
INITSEG  = 0x9000			! we move boot here - out of the way
SETUPSEG = 0x9020			! setup starts here
SYSSEG   = 0x1000			! system loaded at 0x10000 (65536). 8086地址总线是20位
ENDSEG   = SYSSEG + SYSSIZE		! where to stop loading ENDSEG = 0x4000

! ROOT_DEV:	0x000 - same type of floppy as boot.
!		0x301 - first partition on first drive etc
ROOT_DEV = 0x306

entry _start !程序开始
_start: !将0x7c00开始的512字节的内容(bootsect)移动至0x90000处,并跳转
	mov	ax,#BOOTSEG !AX = 0x07c0
	mov	ds,ax !DS = 0x07c0 DS默认段寄存器
	mov	ax,#INITSEG !AX = 0x9000
	mov	es,ax !ES = 0X9000
	mov	cx,#256 !CX = 256
	sub	si,si !SI = SI -SI = 0 源地址DS:SI = 0X07C0:0X0000
	sub	di,di !DI = DI - DI = 0 目的地址ES:DI = 0X9000:0X0000
	rep !重复执行，直到CX = 0
	movw !移动一个字,将DS:SI内容复制至ES:DI
	jmpi	go,INITSEG !间接跳转 跳转到0x9000: go(偏移).此时CS = 0X9000
!实模式下寻址方式为CS<<4 + IP
go:	mov	ax,cs !修改栈顶
	mov	ds,ax
	mov	es,ax !DS = 0X9000 ES = 0X9000
! put stack at 0x9ff00. 汇编里把一段内存空间定义为一个栈，栈总是先进后出，栈的最大空间为 64K。
	mov	ss,ax !任何时刻SS:SP指向栈顶,这里等价于0x90000+0xff00 = 0x9ff00 "栈" 是由高到低使用的
	mov	sp,#0xFF00		! arbitrary value >>512

! load the setup-sectors directly after the bootblock.
! Note that 'es' is already set up.
!从build.c里面可以知道setup.s的大小为4个扇区。大小为4*512byte

! 利用BIOS 中断INT 0x13 将setup 模块从磁盘第2 个扇区
! 开始读到0x90200 开始处，共读4 个扇区。如果读出错，则复位驱动器，并
! 重试，没有退路。INT 0x13 的使用方法如下：
! 读扇区：
! ah = 0x02 - 读磁盘扇区到内存；al = 需要读出的扇区数量；
! ch = 磁道(柱面)号的低8 位； cl = 开始扇区(0-5 位)，磁道号高2 位(6-7)；
! dh = 磁头号； dl = 驱动器号（如果是硬盘则要置位7）；
! es:bx ??指向数据缓冲区； 如果出错则CF 标志置位。
load_setup: !加载setup的内容到bootsect的后面
	mov	dx,#0x0000		! drive 0, head 0 0磁头，0扇区
	mov	cx,#0x0002		! sector 2, track 0 0柱面 扇区2开始
	mov	bx,#0x0200		! address = 512, in INITSEG 起始位置 9000<<4 + 200
	mov	ax,#0x0200+SETUPLEN	! service 2, nr of sectors 读4个扇区
	int	0x13			! read it 
	jnc	ok_load_setup		! ok - continue 读取成功！
	mov	dx,#0x0000
	mov	ax,#0x0000		! reset the diskette
	int	0x13
	j	load_setup !jmp? 重复读取

ok_load_setup: !加载成功

!段寄存器DS指向数据段，ES指向附加段，在存取操作数时，二者之一和一个偏移量合并就可得到存储单元的物理地址。该偏移量可以是具体数值、符号地址和指针寄存器的值等之一，具体情况将由指令的寻址方式来决定。
! Get disk drive parameters, specifically nr of sectors/track
!获取磁盘信息
	mov	dl,#0x00        !DL = drive (bit 7 set for hard disk)
	mov	ax,#0x0800		! AH=8 is get drive parameters
	int	0x13            !seg cs表明了sectors的段地址是cs，而不是ds。而且seg cs 的作用范围只有下一行，不会延生到其他地方。!
	mov	ch,#0x00        !记得sectors那里是.word 0,开始是空的。一个字16位,seg 只会影响下一行
	seg cs              !这里其实就是把获取的CX放到sectors那里，seg是指定段寄存器，其实就等价于mov cs:[sectors] cx
	！保存每磁道最大扇区数。对于软盘，最大磁道号不会超过256，所以CH足以表示，CL[7:6]为0
	mov	sectors,cx      !CH=low eight bits of maximum cylinder number注： cylinder number (bits 8,9 in high bits of CL),CL= maximum sector number (bits 5-0),high two bits of maximum cylinder number (bits 7-6)
	mov	ax,#INITSEG
	mov	es,ax           !ES = 0X9000

! Print some inane message 输出一些信息

	!获取光标位置和形态 注意：这里其实是获取当前光标位置,下面写入并没有修改一些值
	mov	ah,#0x03		! read cursor pos
	xor	bh,bh           !bh = 0x00 bh = page number
	int	0x10            !10号中断是输出信息
	
	mov	cx,#24          !cx = 0x0018这个是输出信息的大小
	mov	bx,#0x0007		! page 0, attribute 7 (normal) 颜色是light gray
	mov	bp,#msg1        !ES:BP points to string to be printed
	mov	ax,#0x1301		! write string, move cursor 这个10 AH=13H的是输出string的中断
	int	0x10

! ok, we've written the message, now
! we want to load the system (at 0x10000)

	mov	ax,#SYSSEG
	mov	es,ax		! segment of 0x010000  es = 0x1000
	call	read_it ！调用read_it子程序，为什么不直接跳转？因为这里会地址入栈，可以RET啊
	call	kill_motor  !关闭马达(???)，反正我之前写的那个貌似不需要做这个事情

! After that we check which root-device to use. If the device is
! defined (!= 0), nothing is done and the given device is used.
! Otherwise, either /dev/PS0 (2,28) or /dev/at0 (2,8), depending
! on the number of sectors that the BIOS reports currently.
!确认根文件系统设备号
!设备号 = (主设备号 << 8) + 次设备号
!软驱的主设备号是 2，次设备号 = type * 4 + nr.
!其中，type 是软驱的类型（比如 2 表示 1.2MB，7 表示 1.44MB）
!nr 等于 0~3 时分别对应软驱 A、B、C、D
!因为是可引导的驱动器，所以肯定是 A 驱，所以 nr = 0;
!对于 1.2MB 的软驱，设备号 = 2 << 8 + 2 * 4 + 0 = 0x208；
!对于1.44MB 的软驱，设备号 = 2 << 8 + 7 * 4 + 0 = 0x21C；
	seg cs
	mov	ax,root_dev  !ax = root_dev = ROOT_DEV = 0x306
	cmp	ax,#0
	jne	root_defined
	seg cs
	mov	bx,sectors
	mov	ax,#0x0208		! /dev/ps0 - 1.2Mb
	cmp	bx,#15          ! 如果等于15，说明是1.2MB的软盘
	je	root_defined
	mov	ax,#0x021c		! /dev/PS0 - 1.44Mb
	cmp	bx,#18          !如果等于18，说明是1.44MB的软盘
	je	root_defined
undef_root:
	jmp undef_root
root_defined:
	seg cs
	mov	root_dev,ax

! after that (everyting loaded), we jump to
! the setup-routine loaded directly after
! the bootblock:

	jmpi	0,SETUPSEG  !间接跳,跳转到 SETUPSEG: 0, 0x9020: 00, 0x90200(setup的地址),间接跳转

! This routine(通常来说) loads the system at address 0x10000, making sure
! no 64kB boundaries are crossed. We try to load it as fast as
! possible, loading whole tracks whenever we can.
!
! in:	es - starting address segment (normally 0x1000)
!
sread:	.word 1+SETUPLEN	! sectors read of current track 0x0005 当前磁道已读了的扇区(5个扇区bootsect占1,setup占4)
head:	.word 0			! current head 磁头
track:	.word 0			! current track 磁道

read_it:
	mov ax,es           !此时es = 0x1000 0x10000 = 64kb
	test ax,#0x0fff     !TEST 是进行按位与 and 这里0x1000 & 0x0fff = 0 test逻辑与运算结果为零,就把ZF(零标志)置1;这里其实就是es必须是0x1000的整数倍
die:	jne die			! es must be at 64kB boundary jne是一个条件转移指令。当ZF=0，转至标号处执行。
	xor bx,bx		! bx is starting address within segment
rp_read:            !重复读取，主要是磁盘读取的方式，先同一圈(柱面、磁道),再换面，再下一圈。
	mov ax,es
	cmp ax,#ENDSEG		! have we loaded ahll yet?
	jb ok1_read         !if ax < #ENDSEG = 0x4000
	ret                 !如果已经把system 0x30000(大小)加载到0x10000 - 0x40000, return
ok1_read:
	seg cs
	mov ax,sectors       !等价于mov ax, cs:[sector] 就是把前面获取的硬件参数放到ax
	sub ax,sread         !SUB 指令从目的操作数中减去源操作数。
	mov cx,ax            !因为扇区数是记录在低6位，前面已经说了，其实软盘CH就足够表示了，6-7是0，这样来左移的时候，也会把柱面的信息溢出，就只有想要的
	shl cx,#9            !逻辑左移 SHL 的第一个操作数是目的操作数，第二个操作数是移位次数：2^9 = 512，这里其实就是计算有多少个扇区的字节还需要读取
	add cx,bx            !这里其实等价于cx + bx < 0x10000 (其实就是没有进位因为要进位，相加结果要达到0x10000)
	jnc ok2_read         !JNC, 当运算没有产生进位标志时,当CF=0时跳转；这里其实等价于 cx + bx < 0x10000
	je ok2_read          !同上 比较两个数相等就跳转，其实就是CF = 0
	xor ax,ax            !否则ax = 0, ax = (0 - bx) / 512 等价于(0x10000 - bx) / 512 注意补码，取反+1
	!举例 2 = 00000010, -2 = 11111110, 0 - 2 = 11111110, 也等于100000000(512) - 0000000010(2) = 0111111110
	sub ax,bx
	shr ax,#9            !注意,这里是逻辑右移不是算数右移
	!上面是计算ax(加上bx[当前0x1000段已经读取的数据])小于0x10000,当前磁道,应该要读取的扇区。
ok2_read:
	call read_track      !调用读扇区过程,al:要读的扇区数(当前柱面[栈道])，es:bx->缓冲区
	mov cx,ax            !cx = ax = 本次读取的扇区数
	add ax,sread         !ax = ax + sread,本磁道已经读取的扇区数
	seg cs               !cs = 0x9000
	cmp ax,sectors       !if ax = sectors
	jne ok3_read         !if ax != sectors(当前磁道扇区未读完) jmp ok3_read
	mov ax,#1            !如果当前磁道扇区已经读完
	sub ax,head          !更换磁头,1 - 0 = 1, 1 - 1 = 0。 
	jne ok4_read         !if head != 1 jmp ok4_read
	inc track            !磁道+1
ok4_read:
	mov head,ax          !head = ax,更新磁道(前面减法就是更换掉磁道了)
	xor ax,ax            !ax = 0, 因为下一个磁道还没开始读，初始值为0
ok3_read:
	mov sread,ax         !更新本磁道已经读取的扇区数
	shl cx,#9
	add bx,cx            !bx = bx + cx * 512,更新相对于0x10000的偏移地址
	jnc rp_read          !如果bx >= 0x10000 jmp re_read(继续读),其实这里不能可能会大于(因为前面就是小于0x10000的),这里产生进位只有一个可能,bx = 0x10000
	mov ax,es            !如果已经读取满0x10000的数据
	add ax,#0x1000       ！es += 0x10000;
	mov es,ax
	xor bx,bx            !bx = 0
	jmp rp_read          !更新es:bx后继续读
!上面其实就是完成的这样的操作,
read_track:          ！读扇区 读取 AL 个扇区到 ES:BX
	push ax
	push bx
	push cx
	push dx          !ax, bx, cx, dx 入栈
	mov dx,track     !当前柱面(磁道)
	mov cx,sread     !当前(已读)扇区
	inc cx           !cx += 1, cl是当前已读扇区
	mov ch,dl        !ch = dl = 当前柱面, cl当前要读的扇区
	mov dx,head
	mov dh,dl        !dh = head 当前磁头
	mov dl,#0        !0号drive
	and dx,#0x0100   !
	mov ah,#2
	int 0x13
	jc bad_rt        !CF=1,出错,复位磁盘
	pop dx           !出栈，还原值，因为ax=本次要读取的扇区数,bx为当前偏移值
	pop cx
	pop bx
	pop ax
	ret
bad_rt:	mov ax,#0
	mov dx,#0
	int 0x13
	pop dx
	pop cx
	pop bx
	pop ax
	jmp read_track

!/*
! * This procedure turns off the floppy drive motor, so
! * that we enter the kernel in a known state, and
! * don't have to worry about it later.
! */
kill_motor:！现在还有软盘么。。。。。虚拟机
	push dx
	mov dx,#0x3f2
	mov al,#0
	outb
	pop dx
	ret

sectors:
	.word 0

msg1:
	.byte 13,10
	.ascii "Loading system ..."
	.byte 13,10,13,10

!查看了一下这个二进制文件，前面确实填充的0
.org 508  !这个应该是伪指令，类似于org 508，其实就类似于从这里开始是508,前面空的是填充0么？
root_dev: !地址 0x901FC
	.word ROOT_DEV !存放根文件系统所在设备号（init/main.c中会用）
boot_flag:
	.word 0xAA55   !引导扇区标志 55aa

.text
endtext:
.data
enddata:
.bss
endbss:

! 这段汇编主要做了以下几件事情
! 把自己复制到0x90000,再跳过去(go)继续往下执行。
! 把setup.s内容(4个扇区)复制到0x90200。
! 读取system模块(0x30000大小)到0x10000处,这个是本文件值得看的。其实还好(写成C肯定容易理解,不过值得称赞的是,汇编里面的一些技巧)。
! 跳转到setup 地址-0x90200
