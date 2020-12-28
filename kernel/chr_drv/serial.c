/*
 *  linux/kernel/serial.c
 *
 *  (C) 1991  Linus Torvalds
 */

/*
 *	serial.c
 *
 * This module implements the rs232 io functions
 *	void rs_write(struct tty_struct * queue);
 *	void rs_init(void);
 * and all interrupts pertaining to serial IO.
 */

#include <linux/tty.h>
#include <linux/sched.h>
#include <asm/system.h>
#include <asm/io.h>

#define WAKEUP_CHARS (TTY_BUF_SIZE/4)

extern void rs1_interrupt(void);
extern void rs2_interrupt(void);

//设置串行端口参数,并测试输出
static void init(int port)
{
	outb_p(0x80,port+3);	/* set DLAB of line control reg */
	outb_p(0x30,port);	/* LS of divisor (48 -> 2400 bps */
	outb_p(0x00,port+1);	/* MS of divisor */
	outb_p(0x03,port+3);	/* reset DLAB */
	outb_p(0x0b,port+4);	/* set DTR,RTS, OUT_2 */
	outb_p(0x0d,port+1);	/* enable all intrs but writes */
	(void)inb(port);	/* read data port to reset things (?) */
}

void rs_init(void)
{
	//设置串口中断处理
	set_intr_gate(0x24,rs1_interrupt);
	set_intr_gate(0x23,rs2_interrupt);
	//详细请看UART和串行(COM)接口,UART一般是RS-232C规格的,它提供了RS-232C数据终端设备接口,这样计算机就可以和调制解调器或其它使用RS-232C接口的串行设备通信了。
	//参考资料：http://www.elecfans.com/emb/jiekou/20171206595886.html
	//主要是将并行数据转换成串行数据流。
	//COM接口是指cluster communication port接口，即串行通讯端口。
	//通常 COM 1 使用的是9 针D 形连接器，也称之为RS-232接口，而COM 2 有的使用的是老式的DB25 针连接器，也称之为RS-422接口，这种接口目前已经很少使用。
	//初始化端口0x3f8,COM1
	init(tty_table[1].read_q.data);
	//初始化端口0x2f8,COM2
	init(tty_table[2].read_q.data);
	//
	outb(inb_p(0x21)&0xE7,0x21);
}

/*
 * This routine gets called when tty_write has put something into
 * the write_queue. It must check wheter the queue is empty, and
 * set the interrupt register accordingly
 *
 *	void _rs_write(struct tty_struct * tty);
 */
void rs_write(struct tty_struct * tty)
{
	cli();
	if (!EMPTY(tty->write_q))
		outb(inb_p(tty->write_q.data+1)|0x02,tty->write_q.data+1);
	sti();
}
