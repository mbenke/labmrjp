#ifdef __APPLE__
.globl _mymain
_mymain:
#else	
.globl mymain
mymain:
#endif
	mov $42, %eax
	ret
