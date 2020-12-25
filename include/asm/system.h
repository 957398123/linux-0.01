#define move_to_user_mode() \
__asm__ ("movl %%esp,%%eax\n\t" \
	"pushl $0x17\n\t" \
	"pushl %%eax\n\t" \
	"pushfl\n\t" \
	"pushl $0x0f\n\t" \
	"pushl $1f\n\t" \
	"iret\n" \
	"1:\tmovl $0x17,%%eax\n\t" \
	"movw %%ax,%%ds\n\t" \
	"movw %%ax,%%es\n\t" \
	"movw %%ax,%%fs\n\t" \
	"movw %%ax,%%gs" \
	:::"ax")

#define sti() __asm__ ("sti"::)
#define cli() __asm__ ("cli"::)
#define nop() __asm__ ("nop"::)

#define iret() __asm__ ("iret"::)

/*
* 设置中断表,等价于setup.s中的setup_idt
* @parm gate_addr 中断向量地址 共256个中断
* @parm type Possible IDT gate types :
* 0b0101	0x5	5	80386 32 bit task gate
* 0b0110	0x6	6	80286 16-bit interrupt gate
* 0b0111	0x7	7	80286 16-bit trap gate
* 0b1110	0xE	14	80386 32-bit interrupt gate 中断
* 0b1111	0xF	15	80386 32-bit trap gate 异常
* @parm dpl  特权级
* @parm addr 处理函数地址
* 对应汇编
* %0,%1,%2...为参数
* movl %3, %edx;
* movl $0x00080000,%eax
* movw %dx, %ax
* movw %0,%dx
* movl %eax, %1
* movl %edx, %2
* Constraint
* a Use the %eax, %ax, or %al registers.
* i  Use an immediate integer value.
* o  Use an offset memory location.
* d  Use the %edx, %dx, or $dl registers.k
*/
#define _set_gate(gate_addr,type,dpl,addr) \
__asm__ ("movw %%dx,%%ax\n\t" \
	"movw %0,%%dx\n\t" \
	"movl %%eax,%1\n\t" \
	"movl %%edx,%2" \
	: \
	: "i" ((short) (0x8000+(dpl<<13)+(type<<8))), \
	"o" (*((char *) (gate_addr))), \
	"o" (*(4+(char *) (gate_addr))), \
	"d" ((char *) (addr)),"a" (0x00080000))

//系统中断
#define set_intr_gate(n,addr) \
	_set_gate(&idt[n],14,0,addr)
//系统异常
#define set_trap_gate(n,addr) \
	_set_gate(&idt[n],15,0,addr)
//用户
#define set_system_gate(n,addr) \
	_set_gate(&idt[n],15,3,addr)

#define _set_seg_desc(gate_addr,type,dpl,base,limit) {\
	*(gate_addr) = ((base) & 0xff000000) | \
		(((base) & 0x00ff0000)>>16) | \
		((limit) & 0xf0000) | \
		((dpl)<<13) | \
		(0x00408000) | \
		((type)<<8); \
	*((gate_addr)+1) = (((base) & 0x0000ffff)<<16) | \
		((limit) & 0x0ffff); }

#define _set_tssldt_desc(n,addr,type) \
__asm__ ("movw $104,%1\n\t" \
	"movw %%ax,%2\n\t" \
	"rorl $16,%%eax\n\t" \
	"movb %%al,%3\n\t" \
	"movb $" type ",%4\n\t" \
	"movb $0x00,%5\n\t" \
	"movb %%ah,%6\n\t" \
	"rorl $16,%%eax" \
	::"a" (addr), "m" (*(n)), "m" (*(n+2)), "m" (*(n+4)), \
	 "m" (*(n+5)), "m" (*(n+6)), "m" (*(n+7)) \
	)

#define set_tss_desc(n,addr) _set_tssldt_desc(((char *) (n)),((int)(addr)),"0x89")
#define set_ldt_desc(n,addr) _set_tssldt_desc(((char *) (n)),((int)(addr)),"0x82")

