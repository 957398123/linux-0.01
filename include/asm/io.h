/*
* IN, OUT指令汇编实现
*/

/*
* 输出值到设备端口
* 函数格式：
* void outb(unsigned char value, unsigned short port);
* @param %0 %eax value 值
* @param %1 %edx port 端口
* 汇编形式
* movl %0, %eax
* movl %1, %edx
* outb %al, %dx
*/
#define outb(value,port) \
__asm__ ("outb %%al,%%dx"::"a" (value),"d" (port))

/*
* inb(port),从指定端口读取1字节的数据
* volatile 关键字表示不需要编译器优化,保持原来汇编的样子。
* _v 返回值,返回的是al里面的值
* 函数格式：
* char inb(unsigned short port);
* @param  %1 %edx port 端口
* @return %0 %eax _v 数据
*/
#define inb(port) ({ \
unsigned char _v; \
__asm__ volatile ("inb %%dx,%%al":"=a" (_v):"d" (port)); \
_v; \
})

/*
* 延迟out postpone
* jmp 1f, 1是标号，f是forward向前，backward向后。
* jmp 1跳转到1那里，然后跳到结束
*/
#define outb_p(value,port) \
__asm__ ("outb %%al,%%dx\n" \
		"\tjmp 1f\n" \
		"1:\tjmp 1f\n" \
		"1:"::"a" (value),"d" (port))

/*
* 延迟in指令 postpone
* 这里读取的时候其实应该要test,确定指令准备好。
*/
#define inb_p(port) ({ \
unsigned char _v; \
__asm__ volatile ("inb %%dx,%%al\n" \
	"\tjmp 1f\n" \
	"1:\tjmp 1f\n" \
	"1:":"=a" (_v):"d" (port)); \
_v; \
})
