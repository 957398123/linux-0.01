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

//���ô��ж˿ڲ���,���������
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
	//���ô����жϴ���
	set_intr_gate(0x24,rs1_interrupt);
	set_intr_gate(0x23,rs2_interrupt);
	//��ϸ�뿴UART�ʹ���(COM)�ӿ�,UARTһ����RS-232C����,���ṩ��RS-232C�����ն��豸�ӿ�,����������Ϳ��Ժ͵��ƽ����������ʹ��RS-232C�ӿڵĴ����豸ͨ���ˡ�
	//�ο����ϣ�http://www.elecfans.com/emb/jiekou/20171206595886.html
	//��Ҫ�ǽ���������ת���ɴ�����������
	//COM�ӿ���ָcluster communication port�ӿڣ�������ͨѶ�˿ڡ�
	//ͨ�� COM 1 ʹ�õ���9 ��D ����������Ҳ��֮ΪRS-232�ӿڣ���COM 2 �е�ʹ�õ�����ʽ��DB25 ����������Ҳ��֮ΪRS-422�ӿڣ����ֽӿ�Ŀǰ�Ѿ�����ʹ�á�
	//��ʼ���˿�0x3f8,COM1
	init(tty_table[1].read_q.data);
	//��ʼ���˿�0x2f8,COM2
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
