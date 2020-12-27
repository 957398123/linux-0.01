/*
* IN, OUTָ����ʵ��
*/

/*
* ���ֵ���豸�˿�
* ������ʽ��
* void outb(unsigned char value, unsigned short port);
* @param %0 %eax value ֵ
* @param %1 %edx port �˿�
* �����ʽ
* movl %0, %eax
* movl %1, %edx
* outb %al, %dx
*/
#define outb(value,port) \
__asm__ ("outb %%al,%%dx"::"a" (value),"d" (port))

/*
* inb(port),��ָ���˿ڶ�ȡ1�ֽڵ�����
* volatile �ؼ��ֱ�ʾ����Ҫ�������Ż�,����ԭ���������ӡ�
* _v ����ֵ,���ص���al�����ֵ
* ������ʽ��
* char inb(unsigned short port);
* @param  %1 %edx port �˿�
* @return %0 %eax _v ����
*/
#define inb(port) ({ \
unsigned char _v; \
__asm__ volatile ("inb %%dx,%%al":"=a" (_v):"d" (port)); \
_v; \
})

/*
* �ӳ�out postpone
* jmp 1f, 1�Ǳ�ţ�f��forward��ǰ��backward���
* jmp 1��ת��1���Ȼ����������
*/
#define outb_p(value,port) \
__asm__ ("outb %%al,%%dx\n" \
		"\tjmp 1f\n" \
		"1:\tjmp 1f\n" \
		"1:"::"a" (value),"d" (port))

/*
* �ӳ�inָ�� postpone
* �����ȡ��ʱ����ʵӦ��Ҫtest,ȷ��ָ��׼���á�
*/
#define inb_p(port) ({ \
unsigned char _v; \
__asm__ volatile ("inb %%dx,%%al\n" \
	"\tjmp 1f\n" \
	"1:\tjmp 1f\n" \
	"1:":"=a" (_v):"d" (port)); \
_v; \
})
