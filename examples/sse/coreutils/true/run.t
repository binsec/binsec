  $ tar xvzf sysroot.tar.gz --one-top-level=sysroot
  bin/true
  usr/lib/debug/.build-id/0e/ea10bd32e512b82055e32b8cb1b9b3e46e4d89.debug
  $ binsec -sse -sse-script run.ini sysroot/bin/true -sse-sysroot sysroot/ -sse-debug-level 2 |
  > grep -e '\[sse:result\]'
  [sse:result] 0x000ff0 lea ecx, [esp + 0x4]     	# <main>
  [sse:result] 0x000ff4 and esp, 0xfffffff0      	# <main> + 0x4
  [sse:result] 0x000ff7 push [ecx + 0xfffffffc]  	# <main> + 0x7
  [sse:result] 0x000ffa push ebp                 	# <main> + 0xa
  [sse:result] 0x000ffb mov ebp, esp             	# <main> + 0xb
  [sse:result] 0x000ffd push edi                 	# <main> + 0xd
  [sse:result] 0x000ffe push esi                 	# <main> + 0xe
  [sse:result] 0x000fff push ebx                 	# <main> + 0xf
  [sse:result] 0x001000 push ecx                 	# <main> + 0x10
  [sse:result] 0x001001 call 0x1110              	# <main> + 0x11
  [sse:result] 0x001110 mov ebx, [esp]           	# <__x86.get_pc_thunk.bx>
  [sse:result] 0x001113 ret                      	# <__x86.get_pc_thunk.bx> + 0x3
  [sse:result] 0x001006 add ebx, 0x6efe          	# <main> + 0x16
  [sse:result] 0x00100c sub esp, 0x8             	# <main> + 0x1c
  [sse:result] 0x00100f cmp [ecx], 0x2           	# <main> + 0x1f
  [sse:result] 0x001012 mov esi, [ecx + 0x4]     	# <main> + 0x22
  [sse:result] 0x001015 jz 0x1025                	# <main> + 0x25
  [sse:result] 0x001017 lea esp, [ebp + 0xfffffff0]	# <main> + 0x27
  [sse:result] 0x00101a xor eax, eax             	# <main> + 0x2a
  [sse:result] 0x00101c pop ecx                  	# <main> + 0x2c
  [sse:result] 0x00101d pop ebx                  	# <main> + 0x2d
  [sse:result] 0x00101e pop esi                  	# <main> + 0x2e
  [sse:result] 0x00101f pop edi                  	# <main> + 0x2f
  [sse:result] 0x001020 pop ebp                  	# <main> + 0x30
  [sse:result] 0x001021 lea esp, [ecx + 0xfffffffc]	# <main> + 0x31
  [sse:result] 0x001024 hook for <main> return   	# <main> + 0x34

