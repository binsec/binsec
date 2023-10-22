type t =
  | S_llseek
  | S_newselect
  | S_sysctl
  | Accept
  | Accept4
  | Access
  | Acct
  | Acl_get
  | Acl_set
  | Add_key
  | Adjtimex
  | Afs_syscall
  | Alarm
  | Alloc_hugepages
  | Arch_prctl
  | Arm_fadvise64_64
  | Arm_sync_file_range
  | Atomic_barrier
  | Atomic_cmpxchg_32
  | Attrctl
  | Bdflush
  | Bind
  | Bpf
  | Break
  | Brk
  | Cacheflush
  | Capget
  | Capset
  | Chdir
  | Chmod
  | Chown
  | Chown32
  | Chroot
  | Clock_adjtime
  | Clock_adjtime64
  | Clock_getres
  | Clock_getres_time64
  | Clock_gettime
  | Clock_gettime64
  | Clock_nanosleep
  | Clock_nanosleep_time64
  | Clock_settime
  | Clock_settime64
  | Clone
  | Clone2
  | Clone3
  | Close
  | Connect
  | Copy_file_range
  | Creat
  | Create_module
  | Delete_module
  | Dup
  | Dup2
  | Dup3
  | Epoll_create
  | Epoll_create1
  | Epoll_ctl
  | Epoll_pwait
  | Epoll_wait
  | Eventfd
  | Eventfd2
  | Execv
  | Execve
  | Execveat
  | Exit
  | Exit_group
  | Faccessat
  | Fadvise64
  | Fadvise64_64
  | Fallocate
  | Fanotify_init
  | Fanotify_mark
  | Fchdir
  | Fchmod
  | Fchmodat
  | Fchown
  | Fchown32
  | Fchownat
  | Fcntl
  | Fcntl64
  | Fdatasync
  | Fgetxattr
  | Finit_module
  | Flistxattr
  | Flock
  | Fork
  | Free_hugepages
  | Fremovexattr
  | Fsconfig
  | Fsetxattr
  | Fsmount
  | Fsopen
  | Fspick
  | Fstat
  | Fstat64
  | Fstatat64
  | Fstatfs
  | Fstatfs64
  | Fsync
  | Ftime
  | Ftruncate
  | Ftruncate64
  | Futex
  | Futex_time64
  | Futimesat
  | Get_kernel_syms
  | Get_mempolicy
  | Get_robust_list
  | Get_thread_area
  | Getcpu
  | Getcwd
  | Getdents
  | Getdents64
  | Getdomainname
  | Getegid
  | Getegid32
  | Geteuid
  | Geteuid32
  | Getgid
  | Getgid32
  | Getgroups
  | Getgroups32
  | Getitimer
  | Getpagesize
  | Getpeername
  | Getpgid
  | Getpgrp
  | Getpid
  | Getpmsg
  | Getppid
  | Getpriority
  | Getrandom
  | Getresgid
  | Getresgid32
  | Getresuid
  | Getresuid32
  | Getrlimit
  | Getrusage
  | Getsid
  | Getsockname
  | Getsockopt
  | Gettid
  | Gettimeofday
  | Getuid
  | Getuid32
  | Getunwind
  | Getxattr
  | Gtty
  | Idle
  | Init_module
  | Inotify_add_watch
  | Inotify_init
  | Inotify_init1
  | Inotify_rm_watch
  | Io_cancel
  | Io_destroy
  | Io_getevents
  | Io_pgetevents
  | Io_pgetevents_time64
  | Io_setup
  | Io_submit
  | Io_uring_enter
  | Io_uring_register
  | Io_uring_setup
  | Ioctl
  | Ioperm
  | Iopl
  | Ioprio_get
  | Ioprio_set
  | Ipc
  | Kcmp
  | Kern_features
  | Kexec_file_load
  | Kexec_load
  | Keyctl
  | Kill
  | Lchown
  | Lchown32
  | Lgetxattr
  | Link
  | Linkat
  | Listen
  | Listxattr
  | Llistxattr
  | Lock
  | Lookup_dcookie
  | Lremovexattr
  | Lseek
  | Lsetxattr
  | Lstat
  | Lstat64
  | Madvise
  | Mbind
  | Membarrier
  | Memfd_create
  | Memory_ordering
  | Migrate_pages
  | Mincore
  | Mkdir
  | Mkdirat
  | Mknod
  | Mknodat
  | Mlock
  | Mlock2
  | Mlockall
  | Mmap
  | Mmap2
  | Modify_ldt
  | Mount
  | Move_mount
  | Move_pages
  | Mprotect
  | Mpx
  | Mq_getsetaddr
  | Mq_getsetattr
  | Mq_notify
  | Mq_open
  | Mq_timedreceive
  | Mq_timedreceive_time64
  | Mq_timedsend
  | Mq_timedsend_time64
  | Mq_unlink
  | Mremap
  | Msgctl
  | Msgget
  | Msgrcv
  | Msgsnd
  | Msync
  | Multiplexer
  | Munlock
  | Munlockall
  | Munmap
  | Name_to_handle_at
  | Nanosleep
  | Newfstatat
  | Nfsservctl
  | Ni_syscall
  | Nice
  | Oldfstat
  | Oldlstat
  | Oldolduname
  | Oldstat
  | Olduname
  | Oldwait4
  | Open
  | Open_by_handle_at
  | Open_tree
  | Openat
  | Openat2
  | Pause
  | Pciconfig_iobase
  | Pciconfig_read
  | Pciconfig_write
  | Perf_event_open
  | Perfctr
  | Perfmonctl
  | Personality
  | Pidfd_getfd
  | Pidfd_open
  | Pidfd_send_signal
  | Pipe
  | Pipe2
  | Pivot_root
  | Pkey_alloc
  | Pkey_free
  | Pkey_mprotect
  | Poll
  | Ppoll
  | Ppoll_time64
  | Prctl
  | Pread64
  | Preadv
  | Preadv2
  | Prlimit64
  | Process_vm_readv
  | Process_vm_writev
  | Prof
  | Profil
  | Pselect6
  | Pselect6_time64
  | Ptrace
  | Putpmsg
  | Pwrite64
  | Pwritev
  | Pwritev2
  | Query_module
  | Quotactl
  | Read
  | Readahead
  | Readdir
  | Readlink
  | Readlinkat
  | Readv
  | Reboot
  | Recv
  | Recvfrom
  | Recvmmsg
  | Recvmmsg_time64
  | Recvmsg
  | Remap_file_pages
  | Removexattr
  | Rename
  | Renameat
  | Renameat2
  | Request_key
  | Restart_syscall
  | Rmdir
  | Rseq
  | Rt_sigaction
  | Rt_sigpending
  | Rt_sigprocmask
  | Rt_sigqueueinfo
  | Rt_sigreturn
  | Rt_sigsuspend
  | Rt_sigtimedwait
  | Rt_sigtimedwait_time64
  | Rt_tgsigqueueinfo
  | Rtas
  | Sched_get_affinity
  | Sched_get_priority_max
  | Sched_get_priority_min
  | Sched_getaffinity
  | Sched_getattr
  | Sched_getparam
  | Sched_getscheduler
  | Sched_rr_get_interval
  | Sched_rr_get_interval_time64
  | Sched_set_affinity
  | Sched_setaffinity
  | Sched_setattr
  | Sched_setparam
  | Sched_setscheduler
  | Sched_yield
  | Seccomp
  | Select
  | Semctl
  | Semget
  | Semop
  | Semtimedop
  | Semtimedop_time64
  | Send
  | Sendfile
  | Sendfile64
  | Sendmmsg
  | Sendmsg
  | Sendto
  | Set_mempolicy
  | Set_robust_list
  | Set_thread_area
  | Set_tid_address
  | Setdomainname
  | Setfsgid
  | Setfsgid32
  | Setfsuid
  | Setfsuid32
  | Setgid
  | Setgid32
  | Setgroups
  | Setgroups32
  | Sethostname
  | Setitimer
  | Setns
  | Setpgid
  | Setpriority
  | Setregid
  | Setregid32
  | Setresgid
  | Setresgid32
  | Setresuid
  | Setresuid32
  | Setreuid
  | Setreuid32
  | Setrlimit
  | Setsid
  | Setsockopt
  | Settimeofday
  | Setuid
  | Setuid32
  | Setxattr
  | Sgetmask
  | Shmat
  | Shmctl
  | Shmdt
  | Shmget
  | Shutdown
  | Sigaction
  | Sigaltstack
  | Signal
  | Signalfd
  | Signalfd4
  | Sigpending
  | Sigprocmask
  | Sigreturn
  | Sigsuspend
  | Socket
  | Socketcall
  | Socketpair
  | Spill
  | Splice
  | Spu_create
  | Spu_run
  | Ssetmask
  | Stat
  | Stat64
  | Statfs
  | Statfs64
  | Statx
  | Stime
  | Stty
  | Subpage_prot
  | Swapcontext
  | Swapoff
  | Swapon
  | Switch_endian
  | Symlink
  | Symlinkat
  | Sync
  | Sync_file_range
  | Sync_file_range2
  | Syncfs
  | Sys_debug_setcontext
  | Syscall
  | Sysfs
  | Sysinfo
  | Syslog
  | Tee
  | Tgkill
  | Time
  | Timer_create
  | Timer_delete
  | Timer_getoverrun
  | Timer_gettime
  | Timer_gettime64
  | Timer_settime
  | Timer_settime64
  | Timerfd
  | Timerfd_create
  | Timerfd_gettime
  | Timerfd_gettime64
  | Timerfd_settime
  | Timerfd_settime64
  | Times
  | Tkill
  | Truncate
  | Truncate64
  | Tuxcall
  | Ugetrlimit
  | Ulimit
  | Umask
  | Umount
  | Umount2
  | Uname
  | Unlink
  | Unlinkat
  | Unshare
  | Uselib
  | Userfaultfd
  | Ustat
  | Utime
  | Utimensat
  | Utimensat_time64
  | Utimes
  | Utrap_install
  | Vfork
  | Vhangup
  | Vm86
  | Vm86old
  | Vmsplice
  | Vserver
  | Wait4
  | Waitid
  | Waitpid
  | Write
  | Writev
  | Xtensa

type descr =
  | Syscall of { name : string; args : (string * string) array }
  | Alias of (string * t)

let description = function
  | S_llseek ->
      Syscall
        {
          name = "_llseek";
          args =
            [|
              ("unsigned int", "fd");
              ("unsigned long", "offset_high");
              ("unsigned long", "offset_low");
              ("loff_t __user *", "result");
              ("unsigned int", "whence");
            |];
        }
  | S_newselect -> Alias ("_newselect", Select)
  | S_sysctl ->
      Syscall
        {
          name = "_sysctl";
          args = [| ("struct __sysctl_args __user *", "args") |];
        }
  | Accept ->
      Syscall
        {
          name = "accept";
          args =
            [|
              ("int", "fd");
              ("struct sockaddr __user *", "upeer_sockaddr");
              ("int __user *", "upeer_addrlen");
            |];
        }
  | Accept4 ->
      Syscall
        {
          name = "accept4";
          args =
            [|
              ("int", "fd");
              ("struct sockaddr __user *", "upeer_sockaddr");
              ("int __user *", "upeer_addrlen");
              ("int", "flags");
            |];
        }
  | Access ->
      Syscall
        {
          name = "access";
          args = [| ("const char __user *", "filename"); ("int", "mode") |];
        }
  | Acct ->
      Syscall { name = "acct"; args = [| ("const char __user *", "name") |] }
  | Acl_get -> Syscall { name = "acl_get"; args = [||] }
  | Acl_set -> Syscall { name = "acl_set"; args = [||] }
  | Add_key ->
      Syscall
        {
          name = "add_key";
          args =
            [|
              ("const char __user *", "_type");
              ("const char __user *", "_description");
              ("const void __user *", "_payload");
              ("size_t", "plen");
              ("key_serial_t", "ringid");
            |];
        }
  | Adjtimex ->
      Syscall
        {
          name = "adjtimex";
          args = [| ("struct __kernel_timex __user *", "txc_p") |];
        }
  | Afs_syscall -> Syscall { name = "afs_syscall"; args = [||] }
  | Alarm ->
      Syscall { name = "alarm"; args = [| ("unsigned int", "seconds") |] }
  | Alloc_hugepages -> Syscall { name = "alloc_hugepages"; args = [||] }
  | Arch_prctl ->
      Syscall
        {
          name = "arch_prctl";
          args = [| ("int", "option"); ("unsigned long", "arg2") |];
        }
  | Arm_fadvise64_64 -> Syscall { name = "arm_fadvise64_64"; args = [||] }
  | Arm_sync_file_range -> Syscall { name = "arm_sync_file_range"; args = [||] }
  | Atomic_barrier -> Syscall { name = "atomic_barrier"; args = [||] }
  | Atomic_cmpxchg_32 -> Syscall { name = "atomic_cmpxchg_32"; args = [||] }
  | Attrctl -> Syscall { name = "attrctl"; args = [||] }
  | Bdflush ->
      Syscall
        { name = "bdflush"; args = [| ("int", "func"); ("long", "data") |] }
  | Bind ->
      Syscall
        {
          name = "bind";
          args =
            [|
              ("int", "fd");
              ("struct sockaddr __user *", "umyaddr");
              ("int", "addrlen");
            |];
        }
  | Bpf ->
      Syscall
        {
          name = "bpf";
          args =
            [|
              ("int", "cmd");
              ("union bpf_attr __user *", "uattr");
              ("unsigned int", "size");
            |];
        }
  | Break -> Syscall { name = "break"; args = [||] }
  | Brk -> Syscall { name = "brk"; args = [| ("unsigned long", "brk") |] }
  | Cacheflush ->
      Syscall
        {
          name = "cacheflush";
          args =
            [|
              ("unsigned long", "addr");
              ("unsigned long", "bytes");
              ("unsigned int", "cache");
            |];
        }
  | Capget ->
      Syscall
        {
          name = "capget";
          args =
            [|
              ("cap_user_header_t", "header"); ("cap_user_data_t", "dataptr");
            |];
        }
  | Capset ->
      Syscall
        {
          name = "capset";
          args =
            [|
              ("cap_user_header_t", "header"); ("const cap_user_data_t", "data");
            |];
        }
  | Chdir ->
      Syscall
        { name = "chdir"; args = [| ("const char __user *", "filename") |] }
  | Chmod ->
      Syscall
        {
          name = "chmod";
          args = [| ("const char __user *", "filename"); ("umode_t", "mode") |];
        }
  | Chown ->
      Syscall
        {
          name = "chown";
          args =
            [|
              ("const char __user *", "filename");
              ("uid_t", "user");
              ("gid_t", "group");
            |];
        }
  | Chown32 -> Alias ("chown32", Chown)
  | Chroot ->
      Syscall
        { name = "chroot"; args = [| ("const char __user *", "filename") |] }
  | Clock_adjtime ->
      Syscall
        {
          name = "clock_adjtime";
          args =
            [|
              ("const clockid_t", "which_clock");
              ("struct __kernel_timex __user *", "utx");
            |];
        }
  | Clock_adjtime64 -> Alias ("clock_adjtime64", Clock_adjtime)
  | Clock_getres ->
      Syscall
        {
          name = "clock_getres";
          args =
            [|
              ("const clockid_t", "which_clock");
              ("struct __kernel_timespec __user *", "tp");
            |];
        }
  | Clock_getres_time64 -> Alias ("clock_getres_time64", Clock_getres)
  | Clock_gettime ->
      Syscall
        {
          name = "clock_gettime";
          args =
            [|
              ("const clockid_t", "which_clock");
              ("struct __kernel_timespec __user *", "tp");
            |];
        }
  | Clock_gettime64 -> Alias ("clock_gettime64", Clock_gettime)
  | Clock_nanosleep ->
      Syscall
        {
          name = "clock_nanosleep";
          args =
            [|
              ("const clockid_t", "which_clock");
              ("int", "flags");
              ("const struct __kernel_timespec __user *", "rqtp");
              ("struct __kernel_timespec __user *", "rmtp");
            |];
        }
  | Clock_nanosleep_time64 -> Alias ("clock_nanosleep_time64", Clock_nanosleep)
  | Clock_settime ->
      Syscall
        {
          name = "clock_settime";
          args =
            [|
              ("const clockid_t", "which_clock");
              ("const struct __kernel_timespec __user *", "tp");
            |];
        }
  | Clock_settime64 -> Alias ("clock_settime64", Clock_settime)
  | Clone ->
      Syscall
        {
          name = "clone";
          args =
            [|
              ("unsigned long", "clone_flags");
              ("unsigned long", "newsp");
              ("int __user *", "parent_tidptr");
              ("unsigned long", "tls");
              ("int __user *", "child_tidptr");
            |];
        }
  | Clone2 -> Syscall { name = "clone2"; args = [||] }
  | Clone3 ->
      Syscall
        {
          name = "clone3";
          args =
            [| ("struct clone_args __user *", "uargs"); ("size_t", "size") |];
        }
  | Close -> Syscall { name = "close"; args = [| ("unsigned int", "fd") |] }
  | Connect ->
      Syscall
        {
          name = "connect";
          args =
            [|
              ("int", "fd");
              ("struct sockaddr __user *", "uservaddr");
              ("int", "addrlen");
            |];
        }
  | Copy_file_range ->
      Syscall
        {
          name = "copy_file_range";
          args =
            [|
              ("int", "fd_in");
              ("loff_t __user *", "off_in");
              ("int", "fd_out");
              ("loff_t __user *", "off_out");
              ("size_t", "len");
              ("unsigned int", "flags");
            |];
        }
  | Creat ->
      Syscall
        {
          name = "creat";
          args = [| ("const char __user *", "pathname"); ("umode_t", "mode") |];
        }
  | Create_module -> Syscall { name = "create_module"; args = [||] }
  | Delete_module ->
      Syscall
        {
          name = "delete_module";
          args =
            [|
              ("const char __user *", "name_user"); ("unsigned int", "flags");
            |];
        }
  | Dup -> Syscall { name = "dup"; args = [| ("unsigned int", "fildes") |] }
  | Dup2 ->
      Syscall
        {
          name = "dup2";
          args = [| ("unsigned int", "oldfd"); ("unsigned int", "newfd") |];
        }
  | Dup3 ->
      Syscall
        {
          name = "dup3";
          args =
            [|
              ("unsigned int", "oldfd");
              ("unsigned int", "newfd");
              ("int", "flags");
            |];
        }
  | Epoll_create ->
      Syscall { name = "epoll_create"; args = [| ("int", "size") |] }
  | Epoll_create1 ->
      Syscall { name = "epoll_create1"; args = [| ("int", "flags") |] }
  | Epoll_ctl ->
      Syscall
        {
          name = "epoll_ctl";
          args =
            [|
              ("int", "epfd");
              ("int", "op");
              ("int", "fd");
              ("struct epoll_event __user *", "event");
            |];
        }
  | Epoll_pwait ->
      Syscall
        {
          name = "epoll_pwait";
          args =
            [|
              ("int", "epfd");
              ("struct epoll_event __user *", "events");
              ("int", "maxevents");
              ("int", "timeout");
              ("const sigset_t __user *", "sigmask");
              ("size_t", "sigsetsize");
            |];
        }
  | Epoll_wait ->
      Syscall
        {
          name = "epoll_wait";
          args =
            [|
              ("int", "epfd");
              ("struct epoll_event __user *", "events");
              ("int", "maxevents");
              ("int", "timeout");
            |];
        }
  | Eventfd ->
      Syscall { name = "eventfd"; args = [| ("unsigned int", "count") |] }
  | Eventfd2 ->
      Syscall
        {
          name = "eventfd2";
          args = [| ("unsigned int", "count"); ("int", "flags") |];
        }
  | Execv -> Syscall { name = "execv"; args = [||] }
  | Execve ->
      Syscall
        {
          name = "execve";
          args =
            [|
              ("const char __user *", "filename");
              ("const char __user *const __user *", "argv");
              ("const char __user *const __user *", "envp");
            |];
        }
  | Execveat ->
      Syscall
        {
          name = "execveat";
          args =
            [|
              ("int", "fd");
              ("const char __user *", "filename");
              ("const char __user *const __user *", "argv");
              ("const char __user *const __user *", "envp");
              ("int", "flags");
            |];
        }
  | Exit -> Syscall { name = "exit"; args = [| ("int", "error_code") |] }
  | Exit_group ->
      Syscall { name = "exit_group"; args = [| ("int", "error_code") |] }
  | Faccessat ->
      Syscall
        {
          name = "faccessat";
          args =
            [|
              ("int", "dfd");
              ("const char __user *", "filename");
              ("int", "mode");
            |];
        }
  | Fadvise64 ->
      Syscall
        {
          name = "fadvise64";
          args =
            [|
              ("int", "fd");
              ("loff_t", "offset");
              ("size_t", "len");
              ("int", "advice");
            |];
        }
  | Fadvise64_64 ->
      Syscall
        {
          name = "fadvise64_64";
          args =
            [|
              ("int", "fd");
              ("loff_t", "offset");
              ("loff_t", "len");
              ("int", "advice");
            |];
        }
  | Fallocate ->
      Syscall
        {
          name = "fallocate";
          args =
            [|
              ("int", "fd");
              ("int", "mode");
              ("loff_t", "offset");
              ("loff_t", "len");
            |];
        }
  | Fanotify_init ->
      Syscall
        {
          name = "fanotify_init";
          args =
            [| ("unsigned int", "flags"); ("unsigned int", "event_f_flags") |];
        }
  | Fanotify_mark ->
      Syscall
        {
          name = "fanotify_mark";
          args =
            [|
              ("int", "fanotify_fd");
              ("unsigned int", "flags");
              ("__u64", "mask");
              ("int", "dfd");
              ("const char __user *", "pathname");
            |];
        }
  | Fchdir -> Syscall { name = "fchdir"; args = [| ("unsigned int", "fd") |] }
  | Fchmod ->
      Syscall
        {
          name = "fchmod";
          args = [| ("unsigned int", "fd"); ("umode_t", "mode") |];
        }
  | Fchmodat ->
      Syscall
        {
          name = "fchmodat";
          args =
            [|
              ("int", "dfd");
              ("const char __user *", "filename");
              ("umode_t", "mode");
            |];
        }
  | Fchown ->
      Syscall
        {
          name = "fchown";
          args =
            [| ("unsigned int", "fd"); ("uid_t", "user"); ("gid_t", "group") |];
        }
  | Fchown32 -> Alias ("fchown32", Fchown)
  | Fchownat ->
      Syscall
        {
          name = "fchownat";
          args =
            [|
              ("int", "dfd");
              ("const char __user *", "filename");
              ("uid_t", "user");
              ("gid_t", "group");
              ("int", "flag");
            |];
        }
  | Fcntl ->
      Syscall
        {
          name = "fcntl";
          args =
            [|
              ("unsigned int", "fd");
              ("unsigned int", "cmd");
              ("unsigned long", "arg");
            |];
        }
  | Fcntl64 ->
      Syscall
        {
          name = "fcntl64";
          args =
            [|
              ("unsigned int", "fd");
              ("unsigned int", "cmd");
              ("unsigned long", "arg");
            |];
        }
  | Fdatasync ->
      Syscall { name = "fdatasync"; args = [| ("unsigned int", "fd") |] }
  | Fgetxattr ->
      Syscall
        {
          name = "fgetxattr";
          args =
            [|
              ("int", "fd");
              ("const char __user *", "name");
              ("void __user *", "value");
              ("size_t", "size");
            |];
        }
  | Finit_module ->
      Syscall
        {
          name = "finit_module";
          args =
            [|
              ("int", "fd"); ("const char __user *", "uargs"); ("int", "flags");
            |];
        }
  | Flistxattr ->
      Syscall
        {
          name = "flistxattr";
          args =
            [| ("int", "fd"); ("char __user *", "list"); ("size_t", "size") |];
        }
  | Flock ->
      Syscall
        {
          name = "flock";
          args = [| ("unsigned int", "fd"); ("unsigned int", "cmd") |];
        }
  | Fork -> Syscall { name = "fork"; args = [||] }
  | Free_hugepages -> Syscall { name = "free_hugepages"; args = [||] }
  | Fremovexattr ->
      Syscall
        {
          name = "fremovexattr";
          args = [| ("int", "fd"); ("const char __user *", "name") |];
        }
  | Fsconfig ->
      Syscall
        {
          name = "fsconfig";
          args =
            [|
              ("int", "fd");
              ("unsigned int", "cmd");
              ("const char __user *", "_key");
              ("const void __user *", "_value");
              ("int", "aux");
            |];
        }
  | Fsetxattr ->
      Syscall
        {
          name = "fsetxattr";
          args =
            [|
              ("int", "fd");
              ("const char __user *", "name");
              ("const void __user *", "value");
              ("size_t", "size");
              ("int", "flags");
            |];
        }
  | Fsmount ->
      Syscall
        {
          name = "fsmount";
          args =
            [|
              ("int", "fs_fd");
              ("unsigned int", "flags");
              ("unsigned int", "attr_flags");
            |];
        }
  | Fsopen ->
      Syscall
        {
          name = "fsopen";
          args =
            [| ("const char __user *", "_fs_name"); ("unsigned int", "flags") |];
        }
  | Fspick ->
      Syscall
        {
          name = "fspick";
          args =
            [|
              ("int", "dfd");
              ("const char __user *", "path");
              ("unsigned int", "flags");
            |];
        }
  | Fstat ->
      Syscall
        {
          name = "fstat";
          args =
            [|
              ("unsigned int", "fd");
              ("struct __old_kernel_stat __user *", "statbuf");
            |];
        }
  | Fstat64 ->
      Syscall
        {
          name = "fstat64";
          args =
            [| ("unsigned long", "fd"); ("struct stat64 __user *", "statbuf") |];
        }
  | Fstatat64 ->
      Syscall
        {
          name = "fstatat64";
          args =
            [|
              ("int", "dfd");
              ("const char __user *", "filename");
              ("struct stat64 __user *", "statbuf");
              ("int", "flag");
            |];
        }
  | Fstatfs ->
      Syscall
        {
          name = "fstatfs";
          args = [| ("unsigned int", "fd"); ("struct statfs __user *", "buf") |];
        }
  | Fstatfs64 ->
      Syscall
        {
          name = "fstatfs64";
          args =
            [|
              ("unsigned int", "fd");
              ("size_t", "sz");
              ("struct statfs64 __user *", "buf");
            |];
        }
  | Fsync -> Syscall { name = "fsync"; args = [| ("unsigned int", "fd") |] }
  | Ftime -> Syscall { name = "ftime"; args = [||] }
  | Ftruncate ->
      Syscall
        {
          name = "ftruncate";
          args = [| ("unsigned int", "fd"); ("unsigned long", "length") |];
        }
  | Ftruncate64 ->
      Syscall
        {
          name = "ftruncate64";
          args = [| ("unsigned int", "fd"); ("loff_t", "length") |];
        }
  | Futex ->
      Syscall
        {
          name = "futex";
          args =
            [|
              ("u32 __user *", "uaddr");
              ("int", "op");
              ("u32", "val");
              ("struct __kernel_timespec __user *", "utime");
              ("u32 __user *", "uaddr2");
              ("u32", "val3");
            |];
        }
  | Futex_time64 -> Alias ("futex_time64", Futex)
  | Futimesat ->
      Syscall
        {
          name = "futimesat";
          args =
            [|
              ("int", "dfd");
              ("const char __user *", "filename");
              ("struct __kernel_old_timeval __user *", "utimes");
            |];
        }
  | Get_kernel_syms -> Syscall { name = "get_kernel_syms"; args = [||] }
  | Get_mempolicy ->
      Syscall
        {
          name = "get_mempolicy";
          args =
            [|
              ("int __user *", "policy");
              ("unsigned long __user *", "nmask");
              ("unsigned long", "maxnode");
              ("unsigned long", "addr");
              ("unsigned long", "flags");
            |];
        }
  | Get_robust_list ->
      Syscall
        {
          name = "get_robust_list";
          args =
            [|
              ("int", "pid");
              ("struct robust_list_head __user * __user *", "head_ptr");
              ("size_t __user *", "len_ptr");
            |];
        }
  | Get_thread_area ->
      Syscall
        {
          name = "get_thread_area";
          args = [| ("struct user_desc __user *", "u_info") |];
        }
  | Getcpu ->
      Syscall
        {
          name = "getcpu";
          args =
            [|
              ("unsigned __user *", "cpup");
              ("unsigned __user *", "nodep");
              ("struct getcpu_cache __user *", "unused");
            |];
        }
  | Getcwd ->
      Syscall
        {
          name = "getcwd";
          args = [| ("char __user *", "buf"); ("unsigned long", "size") |];
        }
  | Getdents ->
      Syscall
        {
          name = "getdents";
          args =
            [|
              ("unsigned int", "fd");
              ("struct linux_dirent __user *", "dirent");
              ("unsigned int", "count");
            |];
        }
  | Getdents64 ->
      Syscall
        {
          name = "getdents64";
          args =
            [|
              ("unsigned int", "fd");
              ("struct linux_dirent64 __user *", "dirent");
              ("unsigned int", "count");
            |];
        }
  | Getdomainname ->
      Syscall
        {
          name = "getdomainname";
          args = [| ("char __user *", "name"); ("int", "len") |];
        }
  | Getegid -> Syscall { name = "getegid"; args = [||] }
  | Getegid32 -> Alias ("getegid32", Getegid)
  | Geteuid -> Syscall { name = "geteuid"; args = [||] }
  | Geteuid32 -> Alias ("geteuid32", Geteuid)
  | Getgid -> Syscall { name = "getgid"; args = [||] }
  | Getgid32 -> Alias ("getgid32", Getgid)
  | Getgroups ->
      Syscall
        {
          name = "getgroups";
          args = [| ("int", "gidsetsize"); ("gid_t __user *", "grouplist") |];
        }
  | Getgroups32 -> Alias ("getgroups32", Getgroups)
  | Getitimer ->
      Syscall
        {
          name = "getitimer";
          args =
            [|
              ("int", "which");
              ("struct __kernel_old_itimerval __user *", "value");
            |];
        }
  | Getpagesize -> Syscall { name = "getpagesize"; args = [||] }
  | Getpeername ->
      Syscall
        {
          name = "getpeername";
          args =
            [|
              ("int", "fd");
              ("struct sockaddr __user *", "usockaddr");
              ("int __user *", "usockaddr_len");
            |];
        }
  | Getpgid -> Syscall { name = "getpgid"; args = [| ("pid_t", "pid") |] }
  | Getpgrp -> Syscall { name = "getpgrp"; args = [||] }
  | Getpid -> Syscall { name = "getpid"; args = [||] }
  | Getpmsg -> Syscall { name = "getpmsg"; args = [||] }
  | Getppid -> Syscall { name = "getppid"; args = [||] }
  | Getpriority ->
      Syscall
        { name = "getpriority"; args = [| ("int", "which"); ("int", "who") |] }
  | Getrandom ->
      Syscall
        {
          name = "getrandom";
          args =
            [|
              ("char __user *", "buf");
              ("size_t", "count");
              ("unsigned int", "flags");
            |];
        }
  | Getresgid ->
      Syscall
        {
          name = "getresgid";
          args =
            [|
              ("gid_t __user *", "rgidp");
              ("gid_t __user *", "egidp");
              ("gid_t __user *", "sgidp");
            |];
        }
  | Getresgid32 -> Alias ("getresgid32", Getresgid)
  | Getresuid ->
      Syscall
        {
          name = "getresuid";
          args =
            [|
              ("uid_t __user *", "ruidp");
              ("uid_t __user *", "euidp");
              ("uid_t __user *", "suidp");
            |];
        }
  | Getresuid32 -> Alias ("getresuid32", Getresuid)
  | Getrlimit ->
      Syscall
        {
          name = "getrlimit";
          args =
            [|
              ("unsigned int", "resource"); ("struct rlimit __user *", "rlim");
            |];
        }
  | Getrusage ->
      Syscall
        {
          name = "getrusage";
          args = [| ("int", "who"); ("struct rusage __user *", "ru") |];
        }
  | Getsid -> Syscall { name = "getsid"; args = [| ("pid_t", "pid") |] }
  | Getsockname ->
      Syscall
        {
          name = "getsockname";
          args =
            [|
              ("int", "fd");
              ("struct sockaddr __user *", "usockaddr");
              ("int __user *", "usockaddr_len");
            |];
        }
  | Getsockopt ->
      Syscall
        {
          name = "getsockopt";
          args =
            [|
              ("int", "fd");
              ("int", "level");
              ("int", "optname");
              ("char __user *", "optval");
              ("int __user *", "optlen");
            |];
        }
  | Gettid -> Syscall { name = "gettid"; args = [||] }
  | Gettimeofday ->
      Syscall
        {
          name = "gettimeofday";
          args =
            [|
              ("struct __kernel_old_timeval __user *", "tv");
              ("struct timezone __user *", "tz");
            |];
        }
  | Getuid -> Syscall { name = "getuid"; args = [||] }
  | Getuid32 -> Alias ("getuid32", Getuid)
  | Getunwind -> Syscall { name = "getunwind"; args = [||] }
  | Getxattr ->
      Syscall
        {
          name = "getxattr";
          args =
            [|
              ("const char __user *", "pathname");
              ("const char __user *", "name");
              ("void __user *", "value");
              ("size_t", "size");
            |];
        }
  | Gtty -> Syscall { name = "gtty"; args = [||] }
  | Idle -> Syscall { name = "idle"; args = [||] }
  | Init_module ->
      Syscall
        {
          name = "init_module";
          args =
            [|
              ("void __user *", "umod");
              ("unsigned long", "len");
              ("const char __user *", "uargs");
            |];
        }
  | Inotify_add_watch ->
      Syscall
        {
          name = "inotify_add_watch";
          args =
            [|
              ("int", "fd"); ("const char __user *", "pathname"); ("u32", "mask");
            |];
        }
  | Inotify_init -> Syscall { name = "inotify_init"; args = [||] }
  | Inotify_init1 ->
      Syscall { name = "inotify_init1"; args = [| ("int", "flags") |] }
  | Inotify_rm_watch ->
      Syscall
        {
          name = "inotify_rm_watch";
          args = [| ("int", "fd"); ("__s32", "wd") |];
        }
  | Io_cancel ->
      Syscall
        {
          name = "io_cancel";
          args =
            [|
              ("aio_context_t", "ctx_id");
              ("struct iocb __user *", "iocb");
              ("struct io_event __user *", "result");
            |];
        }
  | Io_destroy ->
      Syscall { name = "io_destroy"; args = [| ("aio_context_t", "ctx") |] }
  | Io_getevents ->
      Syscall
        {
          name = "io_getevents";
          args =
            [|
              ("aio_context_t", "ctx_id");
              ("long", "min_nr");
              ("long", "nr");
              ("struct io_event __user *", "events");
              ("struct __kernel_timespec __user *", "timeout");
            |];
        }
  | Io_pgetevents ->
      Syscall
        {
          name = "io_pgetevents";
          args =
            [|
              ("aio_context_t", "ctx_id");
              ("long", "min_nr");
              ("long", "nr");
              ("struct io_event __user *", "events");
              ("struct __kernel_timespec __user *", "timeout");
              ("const struct __aio_sigset __user *", "usig");
            |];
        }
  | Io_pgetevents_time64 -> Alias ("io_pgetevents_time64", Io_pgetevents)
  | Io_setup ->
      Syscall
        {
          name = "io_setup";
          args =
            [| ("unsigned", "nr_events"); ("aio_context_t __user *", "ctxp") |];
        }
  | Io_submit ->
      Syscall
        {
          name = "io_submit";
          args =
            [|
              ("aio_context_t", "ctx_id");
              ("long", "nr");
              ("struct iocb __user * __user *", "iocbpp");
            |];
        }
  | Io_uring_enter ->
      Syscall
        {
          name = "io_uring_enter";
          args =
            [|
              ("unsigned int", "fd");
              ("u32", "to_submit");
              ("u32", "min_complete");
              ("u32", "flags");
              ("const sigset_t __user *", "sig");
              ("size_t", "sigsz");
            |];
        }
  | Io_uring_register ->
      Syscall
        {
          name = "io_uring_register";
          args =
            [|
              ("unsigned int", "fd");
              ("unsigned int", "opcode");
              ("void __user *", "arg");
              ("unsigned int", "nr_args");
            |];
        }
  | Io_uring_setup ->
      Syscall
        {
          name = "io_uring_setup";
          args =
            [|
              ("u32", "entries"); ("struct io_uring_params __user *", "params");
            |];
        }
  | Ioctl ->
      Syscall
        {
          name = "ioctl";
          args =
            [|
              ("unsigned int", "fd");
              ("unsigned int", "cmd");
              ("unsigned long", "arg");
            |];
        }
  | Ioperm ->
      Syscall
        {
          name = "ioperm";
          args =
            [|
              ("unsigned long", "from");
              ("unsigned long", "num");
              ("int", "turn_on");
            |];
        }
  | Iopl -> Syscall { name = "iopl"; args = [| ("unsigned int", "level") |] }
  | Ioprio_get ->
      Syscall
        { name = "ioprio_get"; args = [| ("int", "which"); ("int", "who") |] }
  | Ioprio_set ->
      Syscall
        {
          name = "ioprio_set";
          args = [| ("int", "which"); ("int", "who"); ("int", "ioprio") |];
        }
  | Ipc ->
      Syscall
        {
          name = "ipc";
          args =
            [|
              ("unsigned int", "call");
              ("int", "first");
              ("unsigned long", "second");
              ("unsigned long", "third");
              ("void __user *", "ptr");
              ("long", "fifth");
            |];
        }
  | Kcmp ->
      Syscall
        {
          name = "kcmp";
          args =
            [|
              ("pid_t", "pid1");
              ("pid_t", "pid2");
              ("int", "type");
              ("unsigned long", "idx1");
              ("unsigned long", "idx2");
            |];
        }
  | Kern_features -> Syscall { name = "kern_features"; args = [||] }
  | Kexec_file_load ->
      Syscall
        {
          name = "kexec_file_load";
          args =
            [|
              ("int", "kernel_fd");
              ("int", "initrd_fd");
              ("unsigned long", "cmdline_len");
              ("const char __user *", "cmdline_ptr");
              ("unsigned long", "flags");
            |];
        }
  | Kexec_load ->
      Syscall
        {
          name = "kexec_load";
          args =
            [|
              ("unsigned long", "entry");
              ("unsigned long", "nr_segments");
              ("struct kexec_segment __user *", "segments");
              ("unsigned long", "flags");
            |];
        }
  | Keyctl ->
      Syscall
        {
          name = "keyctl";
          args =
            [|
              ("int", "option");
              ("unsigned long", "arg2");
              ("unsigned long", "arg3");
              ("unsigned long", "arg4");
              ("unsigned long", "arg5");
            |];
        }
  | Kill ->
      Syscall { name = "kill"; args = [| ("pid_t", "pid"); ("int", "sig") |] }
  | Lchown ->
      Syscall
        {
          name = "lchown";
          args =
            [|
              ("const char __user *", "filename");
              ("uid_t", "user");
              ("gid_t", "group");
            |];
        }
  | Lchown32 -> Alias ("lchown32", Lchown)
  | Lgetxattr ->
      Syscall
        {
          name = "lgetxattr";
          args =
            [|
              ("const char __user *", "pathname");
              ("const char __user *", "name");
              ("void __user *", "value");
              ("size_t", "size");
            |];
        }
  | Link ->
      Syscall
        {
          name = "link";
          args =
            [|
              ("const char __user *", "oldname");
              ("const char __user *", "newname");
            |];
        }
  | Linkat ->
      Syscall
        {
          name = "linkat";
          args =
            [|
              ("int", "olddfd");
              ("const char __user *", "oldname");
              ("int", "newdfd");
              ("const char __user *", "newname");
              ("int", "flags");
            |];
        }
  | Listen ->
      Syscall
        { name = "listen"; args = [| ("int", "fd"); ("int", "backlog") |] }
  | Listxattr ->
      Syscall
        {
          name = "listxattr";
          args =
            [|
              ("const char __user *", "pathname");
              ("char __user *", "list");
              ("size_t", "size");
            |];
        }
  | Llistxattr ->
      Syscall
        {
          name = "llistxattr";
          args =
            [|
              ("const char __user *", "pathname");
              ("char __user *", "list");
              ("size_t", "size");
            |];
        }
  | Lock -> Syscall { name = "lock"; args = [||] }
  | Lookup_dcookie ->
      Syscall
        {
          name = "lookup_dcookie";
          args =
            [|
              ("u64", "cookie64"); ("char __user *", "buf"); ("size_t", "len");
            |];
        }
  | Lremovexattr ->
      Syscall
        {
          name = "lremovexattr";
          args =
            [|
              ("const char __user *", "pathname");
              ("const char __user *", "name");
            |];
        }
  | Lseek ->
      Syscall
        {
          name = "lseek";
          args =
            [|
              ("unsigned int", "fd");
              ("off_t", "offset");
              ("unsigned int", "whence");
            |];
        }
  | Lsetxattr ->
      Syscall
        {
          name = "lsetxattr";
          args =
            [|
              ("const char __user *", "pathname");
              ("const char __user *", "name");
              ("const void __user *", "value");
              ("size_t", "size");
              ("int", "flags");
            |];
        }
  | Lstat ->
      Syscall
        {
          name = "lstat";
          args =
            [|
              ("const char __user *", "filename");
              ("struct __old_kernel_stat __user *", "statbuf");
            |];
        }
  | Lstat64 ->
      Syscall
        {
          name = "lstat64";
          args =
            [|
              ("const char __user *", "filename");
              ("struct stat64 __user *", "statbuf");
            |];
        }
  | Madvise ->
      Syscall
        {
          name = "madvise";
          args =
            [|
              ("unsigned long", "start");
              ("size_t", "len_in");
              ("int", "behavior");
            |];
        }
  | Mbind ->
      Syscall
        {
          name = "mbind";
          args =
            [|
              ("unsigned long", "start");
              ("unsigned long", "len");
              ("unsigned long", "mode");
              ("const unsigned long __user *", "nmask");
              ("unsigned long", "maxnode");
              ("unsigned int", "flags");
            |];
        }
  | Membarrier ->
      Syscall
        { name = "membarrier"; args = [| ("int", "cmd"); ("int", "flags") |] }
  | Memfd_create ->
      Syscall
        {
          name = "memfd_create";
          args =
            [| ("const char __user *", "uname"); ("unsigned int", "flags") |];
        }
  | Memory_ordering ->
      Syscall
        { name = "memory_ordering"; args = [| ("unsigned long", "model") |] }
  | Migrate_pages ->
      Syscall
        {
          name = "migrate_pages";
          args =
            [|
              ("pid_t", "pid");
              ("unsigned long", "maxnode");
              ("const unsigned long __user *", "old_nodes");
              ("const unsigned long __user *", "new_nodes");
            |];
        }
  | Mincore ->
      Syscall
        {
          name = "mincore";
          args =
            [|
              ("unsigned long", "start");
              ("size_t", "len");
              ("unsigned char __user *", "vec");
            |];
        }
  | Mkdir ->
      Syscall
        {
          name = "mkdir";
          args = [| ("const char __user *", "pathname"); ("umode_t", "mode") |];
        }
  | Mkdirat ->
      Syscall
        {
          name = "mkdirat";
          args =
            [|
              ("int", "dfd");
              ("const char __user *", "pathname");
              ("umode_t", "mode");
            |];
        }
  | Mknod ->
      Syscall
        {
          name = "mknod";
          args =
            [|
              ("const char __user *", "filename");
              ("umode_t", "mode");
              ("unsigned", "dev");
            |];
        }
  | Mknodat ->
      Syscall
        {
          name = "mknodat";
          args =
            [|
              ("int", "dfd");
              ("const char __user *", "filename");
              ("umode_t", "mode");
              ("unsigned int", "dev");
            |];
        }
  | Mlock ->
      Syscall
        {
          name = "mlock";
          args = [| ("unsigned long", "start"); ("size_t", "len") |];
        }
  | Mlock2 ->
      Syscall
        {
          name = "mlock2";
          args =
            [|
              ("unsigned long", "start"); ("size_t", "len"); ("int", "flags");
            |];
        }
  | Mlockall -> Syscall { name = "mlockall"; args = [| ("int", "flags") |] }
  | Mmap ->
      Syscall
        {
          name = "mmap";
          args =
            [|
              ("unsigned long", "addr");
              ("unsigned long", "len");
              ("unsigned long", "prot");
              ("unsigned long", "flags");
              ("unsigned long", "fd");
              ("off_t", "pgoff");
            |];
        }
  | Mmap2 ->
      Syscall
        {
          name = "mmap2";
          args =
            [|
              ("unsigned long", "addr");
              ("unsigned long", "len");
              ("unsigned long", "prot");
              ("unsigned long", "flags");
              ("unsigned long", "fd");
              ("unsigned long", "pgoff");
            |];
        }
  | Modify_ldt ->
      Syscall
        {
          name = "modify_ldt";
          args =
            [|
              ("int", "func");
              ("void __user *", "ptr");
              ("unsigned long", "bytecount");
            |];
        }
  | Mount ->
      Syscall
        {
          name = "mount";
          args =
            [|
              ("char __user *", "dev_name");
              ("char __user *", "dir_name");
              ("char __user *", "type");
              ("unsigned long", "flags");
              ("void __user *", "data");
            |];
        }
  | Move_mount ->
      Syscall
        {
          name = "move_mount";
          args =
            [|
              ("int", "from_dfd");
              ("const char __user *", "from_pathname");
              ("int", "to_dfd");
              ("const char __user *", "to_pathname");
              ("unsigned int", "flags");
            |];
        }
  | Move_pages ->
      Syscall
        {
          name = "move_pages";
          args =
            [|
              ("pid_t", "pid");
              ("unsigned long", "nr_pages");
              ("const void __user * __user *", "pages");
              ("const int __user *", "nodes");
              ("int __user *", "status");
              ("int", "flags");
            |];
        }
  | Mprotect ->
      Syscall
        {
          name = "mprotect";
          args =
            [|
              ("unsigned long", "start");
              ("size_t", "len");
              ("unsigned long", "prot");
            |];
        }
  | Mpx -> Syscall { name = "mpx"; args = [||] }
  | Mq_getsetaddr -> Alias ("mq_getsetaddr", Mq_getsetattr)
  | Mq_getsetattr ->
      Syscall
        {
          name = "mq_getsetattr";
          args =
            [|
              ("mqd_t", "mqdes");
              ("const struct mq_attr __user *", "u_mqstat");
              ("struct mq_attr __user *", "u_omqstat");
            |];
        }
  | Mq_notify ->
      Syscall
        {
          name = "mq_notify";
          args =
            [|
              ("mqd_t", "mqdes");
              ("const struct sigevent __user *", "u_notification");
            |];
        }
  | Mq_open ->
      Syscall
        {
          name = "mq_open";
          args =
            [|
              ("const char __user *", "u_name");
              ("int", "oflag");
              ("umode_t", "mode");
              ("struct mq_attr __user *", "u_attr");
            |];
        }
  | Mq_timedreceive ->
      Syscall
        {
          name = "mq_timedreceive";
          args =
            [|
              ("mqd_t", "mqdes");
              ("char __user *", "u_msg_ptr");
              ("size_t", "msg_len");
              ("unsigned int __user *", "u_msg_prio");
              ("const struct __kernel_timespec __user *", "u_abs_timeout");
            |];
        }
  | Mq_timedreceive_time64 -> Alias ("mq_timedreceive_time64", Mq_timedreceive)
  | Mq_timedsend ->
      Syscall
        {
          name = "mq_timedsend";
          args =
            [|
              ("mqd_t", "mqdes");
              ("const char __user *", "u_msg_ptr");
              ("size_t", "msg_len");
              ("unsigned int", "msg_prio");
              ("const struct __kernel_timespec __user *", "u_abs_timeout");
            |];
        }
  | Mq_timedsend_time64 -> Alias ("mq_timedsend_time64", Mq_timedsend)
  | Mq_unlink ->
      Syscall
        { name = "mq_unlink"; args = [| ("const char __user *", "u_name") |] }
  | Mremap ->
      Syscall
        {
          name = "mremap";
          args =
            [|
              ("unsigned long", "addr");
              ("unsigned long", "old_len");
              ("unsigned long", "new_len");
              ("unsigned long", "flags");
              ("unsigned long", "new_addr");
            |];
        }
  | Msgctl ->
      Syscall
        {
          name = "msgctl";
          args =
            [|
              ("int", "msqid");
              ("int", "cmd");
              ("struct msqid_ds __user *", "buf");
            |];
        }
  | Msgget ->
      Syscall
        { name = "msgget"; args = [| ("key_t", "key"); ("int", "msgflg") |] }
  | Msgrcv ->
      Syscall
        {
          name = "msgrcv";
          args =
            [|
              ("int", "msqid");
              ("struct msgbuf __user *", "msgp");
              ("size_t", "msgsz");
              ("long", "msgtyp");
              ("int", "msgflg");
            |];
        }
  | Msgsnd ->
      Syscall
        {
          name = "msgsnd";
          args =
            [|
              ("int", "msqid");
              ("struct msgbuf __user *", "msgp");
              ("size_t", "msgsz");
              ("int", "msgflg");
            |];
        }
  | Msync ->
      Syscall
        {
          name = "msync";
          args =
            [|
              ("unsigned long", "start"); ("size_t", "len"); ("int", "flags");
            |];
        }
  | Multiplexer -> Syscall { name = "multiplexer"; args = [||] }
  | Munlock ->
      Syscall
        {
          name = "munlock";
          args = [| ("unsigned long", "start"); ("size_t", "len") |];
        }
  | Munlockall -> Syscall { name = "munlockall"; args = [||] }
  | Munmap ->
      Syscall
        {
          name = "munmap";
          args = [| ("unsigned long", "addr"); ("size_t", "len") |];
        }
  | Name_to_handle_at ->
      Syscall
        {
          name = "name_to_handle_at";
          args =
            [|
              ("int", "dfd");
              ("const char __user *", "name");
              ("struct file_handle __user *", "handle");
              ("int __user *", "mnt_id");
              ("int", "flag");
            |];
        }
  | Nanosleep ->
      Syscall
        {
          name = "nanosleep";
          args =
            [|
              ("struct __kernel_timespec __user *", "rqtp");
              ("struct __kernel_timespec __user *", "rmtp");
            |];
        }
  | Newfstatat ->
      Syscall
        {
          name = "newfstatat";
          args =
            [|
              ("int", "dfd");
              ("const char __user *", "filename");
              ("struct stat __user *", "statbuf");
              ("int", "flag");
            |];
        }
  | Nfsservctl -> Syscall { name = "nfsservctl"; args = [||] }
  | Ni_syscall -> Syscall { name = "ni_syscall"; args = [||] }
  | Nice -> Syscall { name = "nice"; args = [| ("int", "increment") |] }
  | Oldfstat -> Alias ("oldfstat", Fstat)
  | Oldlstat -> Alias ("oldlstat", Lstat)
  | Oldolduname -> Alias ("oldolduname", Olduname)
  | Oldstat -> Alias ("oldstat", Stat)
  | Olduname ->
      Syscall
        {
          name = "olduname";
          args = [| ("struct oldold_utsname __user *", "name") |];
        }
  | Oldwait4 -> Syscall { name = "oldwait4"; args = [||] }
  | Open ->
      Syscall
        {
          name = "open";
          args =
            [|
              ("const char __user *", "filename");
              ("int", "flags");
              ("umode_t", "mode");
            |];
        }
  | Open_by_handle_at ->
      Syscall
        {
          name = "open_by_handle_at";
          args =
            [|
              ("int", "mountdirfd");
              ("struct file_handle __user *", "handle");
              ("int", "flags");
            |];
        }
  | Open_tree ->
      Syscall
        {
          name = "open_tree";
          args =
            [|
              ("int", "dfd");
              ("const char __user *", "filename");
              ("unsigned", "flags");
            |];
        }
  | Openat ->
      Syscall
        {
          name = "openat";
          args =
            [|
              ("int", "dfd");
              ("const char __user *", "filename");
              ("int", "flags");
              ("umode_t", "mode");
            |];
        }
  | Openat2 ->
      Syscall
        {
          name = "openat2";
          args =
            [|
              ("int", "dfd");
              ("const char __user *", "filename");
              ("struct open_how __user *", "how");
              ("size_t", "usize");
            |];
        }
  | Pause -> Syscall { name = "pause"; args = [||] }
  | Pciconfig_iobase ->
      Syscall
        {
          name = "pciconfig_iobase";
          args =
            [|
              ("long", "which");
              ("unsigned long", "bus");
              ("unsigned long", "devfn");
            |];
        }
  | Pciconfig_read ->
      Syscall
        {
          name = "pciconfig_read";
          args =
            [|
              ("unsigned long", "bus");
              ("unsigned long", "dfn");
              ("unsigned long", "off");
              ("unsigned long", "len");
              ("void __user *", "buf");
            |];
        }
  | Pciconfig_write ->
      Syscall
        {
          name = "pciconfig_write";
          args =
            [|
              ("unsigned long", "bus");
              ("unsigned long", "dfn");
              ("unsigned long", "off");
              ("unsigned long", "len");
              ("void __user *", "buf");
            |];
        }
  | Perf_event_open ->
      Syscall
        {
          name = "perf_event_open";
          args =
            [|
              ("struct perf_event_attr __user *", "attr_uptr");
              ("pid_t", "pid");
              ("int", "cpu");
              ("int", "group_fd");
              ("unsigned long", "flags");
            |];
        }
  | Perfctr -> Syscall { name = "perfctr"; args = [||] }
  | Perfmonctl -> Syscall { name = "perfmonctl"; args = [||] }
  | Personality ->
      Syscall
        { name = "personality"; args = [| ("unsigned int", "personality") |] }
  | Pidfd_getfd ->
      Syscall
        {
          name = "pidfd_getfd";
          args =
            [| ("int", "pidfd"); ("int", "fd"); ("unsigned int", "flags") |];
        }
  | Pidfd_open ->
      Syscall
        {
          name = "pidfd_open";
          args = [| ("pid_t", "pid"); ("unsigned int", "flags") |];
        }
  | Pidfd_send_signal ->
      Syscall
        {
          name = "pidfd_send_signal";
          args =
            [|
              ("int", "pidfd");
              ("int", "sig");
              ("siginfo_t __user *", "info");
              ("unsigned int", "flags");
            |];
        }
  | Pipe -> Syscall { name = "pipe"; args = [| ("int __user *", "fildes") |] }
  | Pipe2 ->
      Syscall
        {
          name = "pipe2";
          args = [| ("int __user *", "fildes"); ("int", "flags") |];
        }
  | Pivot_root ->
      Syscall
        {
          name = "pivot_root";
          args =
            [|
              ("const char __user *", "new_root");
              ("const char __user *", "put_old");
            |];
        }
  | Pkey_alloc ->
      Syscall
        {
          name = "pkey_alloc";
          args = [| ("unsigned long", "flags"); ("unsigned long", "init_val") |];
        }
  | Pkey_free -> Syscall { name = "pkey_free"; args = [| ("int", "pkey") |] }
  | Pkey_mprotect ->
      Syscall
        {
          name = "pkey_mprotect";
          args =
            [|
              ("unsigned long", "start");
              ("size_t", "len");
              ("unsigned long", "prot");
              ("int", "pkey");
            |];
        }
  | Poll ->
      Syscall
        {
          name = "poll";
          args =
            [|
              ("struct pollfd __user *", "ufds");
              ("unsigned int", "nfds");
              ("int", "timeout_msecs");
            |];
        }
  | Ppoll ->
      Syscall
        {
          name = "ppoll";
          args =
            [|
              ("struct pollfd __user *", "ufds");
              ("unsigned int", "nfds");
              ("struct __kernel_timespec __user *", "tsp");
              ("const sigset_t __user *", "sigmask");
              ("size_t", "sigsetsize");
            |];
        }
  | Ppoll_time64 -> Alias ("ppoll_time64", Ppoll)
  | Prctl ->
      Syscall
        {
          name = "prctl";
          args =
            [|
              ("int", "option");
              ("unsigned long", "arg2");
              ("unsigned long", "arg3");
              ("unsigned long", "arg4");
              ("unsigned long", "arg5");
            |];
        }
  | Pread64 ->
      Syscall
        {
          name = "pread64";
          args =
            [|
              ("unsigned int", "fd");
              ("char __user *", "buf");
              ("size_t", "count");
              ("loff_t", "pos");
            |];
        }
  | Preadv ->
      Syscall
        {
          name = "preadv";
          args =
            [|
              ("unsigned long", "fd");
              ("const struct iovec __user *", "vec");
              ("unsigned long", "vlen");
              ("unsigned long", "pos_l");
              ("unsigned long", "pos_h");
            |];
        }
  | Preadv2 ->
      Syscall
        {
          name = "preadv2";
          args =
            [|
              ("unsigned long", "fd");
              ("const struct iovec __user *", "vec");
              ("unsigned long", "vlen");
              ("unsigned long", "pos_l");
              ("unsigned long", "pos_h");
              ("rwf_t", "flags");
            |];
        }
  | Prlimit64 ->
      Syscall
        {
          name = "prlimit64";
          args =
            [|
              ("pid_t", "pid");
              ("unsigned int", "resource");
              ("const struct rlimit64 __user *", "new_rlim");
              ("struct rlimit64 __user *", "old_rlim");
            |];
        }
  | Process_vm_readv ->
      Syscall
        {
          name = "process_vm_readv";
          args =
            [|
              ("pid_t", "pid");
              ("const struct iovec __user *", "lvec");
              ("unsigned long", "liovcnt");
              ("const struct iovec __user *", "rvec");
              ("unsigned long", "riovcnt");
              ("unsigned long", "flags");
            |];
        }
  | Process_vm_writev ->
      Syscall
        {
          name = "process_vm_writev";
          args =
            [|
              ("pid_t", "pid");
              ("const struct iovec __user *", "lvec");
              ("unsigned long", "liovcnt");
              ("const struct iovec __user *", "rvec");
              ("unsigned long", "riovcnt");
              ("unsigned long", "flags");
            |];
        }
  | Prof -> Syscall { name = "prof"; args = [||] }
  | Profil -> Syscall { name = "profil"; args = [||] }
  | Pselect6 ->
      Syscall
        {
          name = "pselect6";
          args =
            [|
              ("int", "n");
              ("fd_set __user *", "inp");
              ("fd_set __user *", "outp");
              ("fd_set __user *", "exp");
              ("struct __kernel_timespec __user *", "tsp");
              ("void __user *", "sig");
            |];
        }
  | Pselect6_time64 -> Alias ("pselect6_time64", Pselect6)
  | Ptrace ->
      Syscall
        {
          name = "ptrace";
          args =
            [|
              ("long", "request");
              ("long", "pid");
              ("unsigned long", "addr");
              ("unsigned long", "data");
            |];
        }
  | Putpmsg -> Syscall { name = "putpmsg"; args = [||] }
  | Pwrite64 ->
      Syscall
        {
          name = "pwrite64";
          args =
            [|
              ("unsigned int", "fd");
              ("const char __user *", "buf");
              ("size_t", "count");
              ("loff_t", "pos");
            |];
        }
  | Pwritev ->
      Syscall
        {
          name = "pwritev";
          args =
            [|
              ("unsigned long", "fd");
              ("const struct iovec __user *", "vec");
              ("unsigned long", "vlen");
              ("unsigned long", "pos_l");
              ("unsigned long", "pos_h");
            |];
        }
  | Pwritev2 ->
      Syscall
        {
          name = "pwritev2";
          args =
            [|
              ("unsigned long", "fd");
              ("const struct iovec __user *", "vec");
              ("unsigned long", "vlen");
              ("unsigned long", "pos_l");
              ("unsigned long", "pos_h");
              ("rwf_t", "flags");
            |];
        }
  | Query_module -> Syscall { name = "query_module"; args = [||] }
  | Quotactl ->
      Syscall
        {
          name = "quotactl";
          args =
            [|
              ("unsigned int", "cmd");
              ("const char __user *", "special");
              ("qid_t", "id");
              ("void __user *", "addr");
            |];
        }
  | Read ->
      Syscall
        {
          name = "read";
          args =
            [|
              ("unsigned int", "fd");
              ("char __user *", "buf");
              ("size_t", "count");
            |];
        }
  | Readahead ->
      Syscall
        {
          name = "readahead";
          args = [| ("int", "fd"); ("loff_t", "offset"); ("size_t", "count") |];
        }
  | Readdir ->
      Syscall
        {
          name = "readdir";
          args =
            [|
              ("unsigned int", "fd");
              ("struct old_linux_dirent __user *", "dirent");
              ("unsigned int", "count");
            |];
        }
  | Readlink ->
      Syscall
        {
          name = "readlink";
          args =
            [|
              ("const char __user *", "path");
              ("char __user *", "buf");
              ("int", "bufsiz");
            |];
        }
  | Readlinkat ->
      Syscall
        {
          name = "readlinkat";
          args =
            [|
              ("int", "dfd");
              ("const char __user *", "pathname");
              ("char __user *", "buf");
              ("int", "bufsiz");
            |];
        }
  | Readv ->
      Syscall
        {
          name = "readv";
          args =
            [|
              ("unsigned long", "fd");
              ("const struct iovec __user *", "vec");
              ("unsigned long", "vlen");
            |];
        }
  | Reboot ->
      Syscall
        {
          name = "reboot";
          args =
            [|
              ("int", "magic1");
              ("int", "magic2");
              ("unsigned int", "cmd");
              ("void __user *", "arg");
            |];
        }
  | Recv ->
      Syscall
        {
          name = "recv";
          args =
            [|
              ("int", "fd");
              ("void __user *", "ubuf");
              ("size_t", "size");
              ("unsigned int", "flags");
            |];
        }
  | Recvfrom ->
      Syscall
        {
          name = "recvfrom";
          args =
            [|
              ("int", "fd");
              ("void __user *", "ubuf");
              ("size_t", "size");
              ("unsigned int", "flags");
              ("struct sockaddr __user *", "addr");
              ("int __user *", "addr_len");
            |];
        }
  | Recvmmsg ->
      Syscall
        {
          name = "recvmmsg";
          args =
            [|
              ("int", "fd");
              ("struct mmsghdr __user *", "mmsg");
              ("unsigned int", "vlen");
              ("unsigned int", "flags");
              ("struct __kernel_timespec __user *", "timeout");
            |];
        }
  | Recvmmsg_time64 -> Alias ("recvmmsg_time64", Recvmmsg)
  | Recvmsg ->
      Syscall
        {
          name = "recvmsg";
          args =
            [|
              ("int", "fd");
              ("struct user_msghdr __user *", "msg");
              ("unsigned int", "flags");
            |];
        }
  | Remap_file_pages ->
      Syscall
        {
          name = "remap_file_pages";
          args =
            [|
              ("unsigned long", "start");
              ("unsigned long", "size");
              ("unsigned long", "prot");
              ("unsigned long", "pgoff");
              ("unsigned long", "flags");
            |];
        }
  | Removexattr ->
      Syscall
        {
          name = "removexattr";
          args =
            [|
              ("const char __user *", "pathname");
              ("const char __user *", "name");
            |];
        }
  | Rename ->
      Syscall
        {
          name = "rename";
          args =
            [|
              ("const char __user *", "oldname");
              ("const char __user *", "newname");
            |];
        }
  | Renameat ->
      Syscall
        {
          name = "renameat";
          args =
            [|
              ("int", "olddfd");
              ("const char __user *", "oldname");
              ("int", "newdfd");
              ("const char __user *", "newname");
            |];
        }
  | Renameat2 ->
      Syscall
        {
          name = "renameat2";
          args =
            [|
              ("int", "olddfd");
              ("const char __user *", "oldname");
              ("int", "newdfd");
              ("const char __user *", "newname");
              ("unsigned int", "flags");
            |];
        }
  | Request_key ->
      Syscall
        {
          name = "request_key";
          args =
            [|
              ("const char __user *", "_type");
              ("const char __user *", "_description");
              ("const char __user *", "_callout_info");
              ("key_serial_t", "destringid");
            |];
        }
  | Restart_syscall -> Syscall { name = "restart_syscall"; args = [||] }
  | Rmdir ->
      Syscall
        { name = "rmdir"; args = [| ("const char __user *", "pathname") |] }
  | Rseq ->
      Syscall
        {
          name = "rseq";
          args =
            [|
              ("struct rseq __user *", "rseq");
              ("u32", "rseq_len");
              ("int", "flags");
              ("u32", "sig");
            |];
        }
  | Rt_sigaction ->
      Syscall
        {
          name = "rt_sigaction";
          args =
            [|
              ("int", "sig");
              ("const struct sigaction __user *", "act");
              ("struct sigaction __user *", "oact");
              ("void __user *", "restorer");
              ("size_t", "sigsetsize");
            |];
        }
  | Rt_sigpending ->
      Syscall
        {
          name = "rt_sigpending";
          args = [| ("sigset_t __user *", "uset"); ("size_t", "sigsetsize") |];
        }
  | Rt_sigprocmask ->
      Syscall
        {
          name = "rt_sigprocmask";
          args =
            [|
              ("int", "how");
              ("sigset_t __user *", "nset");
              ("sigset_t __user *", "oset");
              ("size_t", "sigsetsize");
            |];
        }
  | Rt_sigqueueinfo ->
      Syscall
        {
          name = "rt_sigqueueinfo";
          args =
            [|
              ("pid_t", "pid"); ("int", "sig"); ("siginfo_t __user *", "uinfo");
            |];
        }
  | Rt_sigreturn -> Syscall { name = "rt_sigreturn"; args = [||] }
  | Rt_sigsuspend ->
      Syscall
        {
          name = "rt_sigsuspend";
          args =
            [| ("sigset_t __user *", "unewset"); ("size_t", "sigsetsize") |];
        }
  | Rt_sigtimedwait ->
      Syscall
        {
          name = "rt_sigtimedwait";
          args =
            [|
              ("const sigset_t __user *", "uthese");
              ("siginfo_t __user *", "uinfo");
              ("const struct __kernel_timespec __user *", "uts");
              ("size_t", "sigsetsize");
            |];
        }
  | Rt_sigtimedwait_time64 -> Alias ("rt_sigtimedwait_time64", Rt_sigtimedwait)
  | Rt_tgsigqueueinfo ->
      Syscall
        {
          name = "rt_tgsigqueueinfo";
          args =
            [|
              ("pid_t", "tgid");
              ("pid_t", "pid");
              ("int", "sig");
              ("siginfo_t __user *", "uinfo");
            |];
        }
  | Rtas ->
      Syscall
        { name = "rtas"; args = [| ("struct rtas_args __user *", "uargs") |] }
  | Sched_get_affinity -> Syscall { name = "sched_get_affinity"; args = [||] }
  | Sched_get_priority_max ->
      Syscall
        { name = "sched_get_priority_max"; args = [| ("int", "policy") |] }
  | Sched_get_priority_min ->
      Syscall
        { name = "sched_get_priority_min"; args = [| ("int", "policy") |] }
  | Sched_getaffinity ->
      Syscall
        {
          name = "sched_getaffinity";
          args =
            [|
              ("pid_t", "pid");
              ("unsigned int", "len");
              ("unsigned long __user *", "user_mask_ptr");
            |];
        }
  | Sched_getattr ->
      Syscall
        {
          name = "sched_getattr";
          args =
            [|
              ("pid_t", "pid");
              ("struct sched_attr __user *", "uattr");
              ("unsigned int", "usize");
              ("unsigned int", "flags");
            |];
        }
  | Sched_getparam ->
      Syscall
        {
          name = "sched_getparam";
          args =
            [| ("pid_t", "pid"); ("struct sched_param __user *", "param") |];
        }
  | Sched_getscheduler ->
      Syscall { name = "sched_getscheduler"; args = [| ("pid_t", "pid") |] }
  | Sched_rr_get_interval ->
      Syscall
        {
          name = "sched_rr_get_interval";
          args =
            [|
              ("pid_t", "pid"); ("struct __kernel_timespec __user *", "interval");
            |];
        }
  | Sched_rr_get_interval_time64 ->
      Alias ("sched_rr_get_interval_time64", Sched_rr_get_interval)
  | Sched_set_affinity -> Syscall { name = "sched_set_affinity"; args = [||] }
  | Sched_setaffinity ->
      Syscall
        {
          name = "sched_setaffinity";
          args =
            [|
              ("pid_t", "pid");
              ("unsigned int", "len");
              ("unsigned long __user *", "user_mask_ptr");
            |];
        }
  | Sched_setattr ->
      Syscall
        {
          name = "sched_setattr";
          args =
            [|
              ("pid_t", "pid");
              ("struct sched_attr __user *", "uattr");
              ("unsigned int", "flags");
            |];
        }
  | Sched_setparam ->
      Syscall
        {
          name = "sched_setparam";
          args =
            [| ("pid_t", "pid"); ("struct sched_param __user *", "param") |];
        }
  | Sched_setscheduler ->
      Syscall
        {
          name = "sched_setscheduler";
          args =
            [|
              ("pid_t", "pid");
              ("int", "policy");
              ("struct sched_param __user *", "param");
            |];
        }
  | Sched_yield -> Syscall { name = "sched_yield"; args = [||] }
  | Seccomp ->
      Syscall
        {
          name = "seccomp";
          args =
            [|
              ("unsigned int", "op");
              ("unsigned int", "flags");
              ("void __user *", "uargs");
            |];
        }
  | Select ->
      Syscall
        {
          name = "select";
          args =
            [|
              ("int", "n");
              ("fd_set __user *", "inp");
              ("fd_set __user *", "outp");
              ("fd_set __user *", "exp");
              ("struct __kernel_old_timeval __user *", "tvp");
            |];
        }
  | Semctl ->
      Syscall
        {
          name = "semctl";
          args =
            [|
              ("int", "semid");
              ("int", "semnum");
              ("int", "cmd");
              ("unsigned long", "arg");
            |];
        }
  | Semget ->
      Syscall
        {
          name = "semget";
          args = [| ("key_t", "key"); ("int", "nsems"); ("int", "semflg") |];
        }
  | Semop ->
      Syscall
        {
          name = "semop";
          args =
            [|
              ("int", "semid");
              ("struct sembuf __user *", "tsops");
              ("unsigned", "nsops");
            |];
        }
  | Semtimedop ->
      Syscall
        {
          name = "semtimedop";
          args =
            [|
              ("int", "semid");
              ("struct sembuf __user *", "tsops");
              ("unsigned int", "nsops");
              ("const struct __kernel_timespec __user *", "timeout");
            |];
        }
  | Semtimedop_time64 -> Alias ("semtimedop_time64", Semtimedop)
  | Send ->
      Syscall
        {
          name = "send";
          args =
            [|
              ("int", "fd");
              ("void __user *", "buff");
              ("size_t", "len");
              ("unsigned int", "flags");
            |];
        }
  | Sendfile ->
      Syscall
        {
          name = "sendfile";
          args =
            [|
              ("int", "out_fd");
              ("int", "in_fd");
              ("off_t __user *", "offset");
              ("size_t", "count");
            |];
        }
  | Sendfile64 ->
      Syscall
        {
          name = "sendfile64";
          args =
            [|
              ("int", "out_fd");
              ("int", "in_fd");
              ("loff_t __user *", "offset");
              ("size_t", "count");
            |];
        }
  | Sendmmsg ->
      Syscall
        {
          name = "sendmmsg";
          args =
            [|
              ("int", "fd");
              ("struct mmsghdr __user *", "mmsg");
              ("unsigned int", "vlen");
              ("unsigned int", "flags");
            |];
        }
  | Sendmsg ->
      Syscall
        {
          name = "sendmsg";
          args =
            [|
              ("int", "fd");
              ("struct user_msghdr __user *", "msg");
              ("unsigned int", "flags");
            |];
        }
  | Sendto ->
      Syscall
        {
          name = "sendto";
          args =
            [|
              ("int", "fd");
              ("void __user *", "buff");
              ("size_t", "len");
              ("unsigned int", "flags");
              ("struct sockaddr __user *", "addr");
              ("int", "addr_len");
            |];
        }
  | Set_mempolicy ->
      Syscall
        {
          name = "set_mempolicy";
          args =
            [|
              ("int", "mode");
              ("const unsigned long __user *", "nmask");
              ("unsigned long", "maxnode");
            |];
        }
  | Set_robust_list ->
      Syscall
        {
          name = "set_robust_list";
          args =
            [|
              ("struct robust_list_head __user *", "head"); ("size_t", "len");
            |];
        }
  | Set_thread_area ->
      Syscall
        { name = "set_thread_area"; args = [| ("unsigned long", "addr") |] }
  | Set_tid_address ->
      Syscall
        { name = "set_tid_address"; args = [| ("int __user *", "tidptr") |] }
  | Setdomainname ->
      Syscall
        {
          name = "setdomainname";
          args = [| ("char __user *", "name"); ("int", "len") |];
        }
  | Setfsgid -> Syscall { name = "setfsgid"; args = [| ("gid_t", "gid") |] }
  | Setfsgid32 -> Alias ("setfsgid32", Setfsgid)
  | Setfsuid -> Syscall { name = "setfsuid"; args = [| ("uid_t", "uid") |] }
  | Setfsuid32 -> Alias ("setfsuid32", Setfsuid)
  | Setgid -> Syscall { name = "setgid"; args = [| ("gid_t", "gid") |] }
  | Setgid32 -> Alias ("setgid32", Setgid)
  | Setgroups ->
      Syscall
        {
          name = "setgroups";
          args = [| ("int", "gidsetsize"); ("gid_t __user *", "grouplist") |];
        }
  | Setgroups32 -> Alias ("setgroups32", Setgroups)
  | Sethostname ->
      Syscall
        {
          name = "sethostname";
          args = [| ("char __user *", "name"); ("int", "len") |];
        }
  | Setitimer ->
      Syscall
        {
          name = "setitimer";
          args =
            [|
              ("int", "which");
              ("struct __kernel_old_itimerval __user *", "value");
              ("struct __kernel_old_itimerval __user *", "ovalue");
            |];
        }
  | Setns ->
      Syscall { name = "setns"; args = [| ("int", "fd"); ("int", "nstype") |] }
  | Setpgid ->
      Syscall
        { name = "setpgid"; args = [| ("pid_t", "pid"); ("pid_t", "pgid") |] }
  | Setpriority ->
      Syscall
        {
          name = "setpriority";
          args = [| ("int", "which"); ("int", "who"); ("int", "niceval") |];
        }
  | Setregid ->
      Syscall
        { name = "setregid"; args = [| ("gid_t", "rgid"); ("gid_t", "egid") |] }
  | Setregid32 -> Alias ("setregid32", Setregid)
  | Setresgid ->
      Syscall
        {
          name = "setresgid";
          args = [| ("gid_t", "rgid"); ("gid_t", "egid"); ("gid_t", "sgid") |];
        }
  | Setresgid32 -> Alias ("setresgid32", Setresgid)
  | Setresuid ->
      Syscall
        {
          name = "setresuid";
          args = [| ("uid_t", "ruid"); ("uid_t", "euid"); ("uid_t", "suid") |];
        }
  | Setresuid32 -> Alias ("setresuid32", Setresuid)
  | Setreuid ->
      Syscall
        { name = "setreuid"; args = [| ("uid_t", "ruid"); ("uid_t", "euid") |] }
  | Setreuid32 -> Alias ("setreuid32", Setreuid)
  | Setrlimit ->
      Syscall
        {
          name = "setrlimit";
          args =
            [|
              ("unsigned int", "resource"); ("struct rlimit __user *", "rlim");
            |];
        }
  | Setsid -> Syscall { name = "setsid"; args = [||] }
  | Setsockopt ->
      Syscall
        {
          name = "setsockopt";
          args =
            [|
              ("int", "fd");
              ("int", "level");
              ("int", "optname");
              ("char __user *", "optval");
              ("int", "optlen");
            |];
        }
  | Settimeofday ->
      Syscall
        {
          name = "settimeofday";
          args =
            [|
              ("struct __kernel_old_timeval __user *", "tv");
              ("struct timezone __user *", "tz");
            |];
        }
  | Setuid -> Syscall { name = "setuid"; args = [| ("uid_t", "uid") |] }
  | Setuid32 -> Alias ("setuid32", Setuid)
  | Setxattr ->
      Syscall
        {
          name = "setxattr";
          args =
            [|
              ("const char __user *", "pathname");
              ("const char __user *", "name");
              ("const void __user *", "value");
              ("size_t", "size");
              ("int", "flags");
            |];
        }
  | Sgetmask -> Syscall { name = "sgetmask"; args = [||] }
  | Shmat ->
      Syscall
        {
          name = "shmat";
          args =
            [|
              ("int", "shmid"); ("char __user *", "shmaddr"); ("int", "shmflg");
            |];
        }
  | Shmctl ->
      Syscall
        {
          name = "shmctl";
          args =
            [|
              ("int", "shmid");
              ("int", "cmd");
              ("struct shmid_ds __user *", "buf");
            |];
        }
  | Shmdt ->
      Syscall { name = "shmdt"; args = [| ("char __user *", "shmaddr") |] }
  | Shmget ->
      Syscall
        {
          name = "shmget";
          args = [| ("key_t", "key"); ("size_t", "size"); ("int", "shmflg") |];
        }
  | Shutdown ->
      Syscall { name = "shutdown"; args = [| ("int", "fd"); ("int", "how") |] }
  | Sigaction ->
      Syscall
        {
          name = "sigaction";
          args =
            [|
              ("int", "sig");
              ("const struct sigaction __user *", "act");
              ("struct sigaction __user *", "oact");
            |];
        }
  | Sigaltstack ->
      Syscall
        {
          name = "sigaltstack";
          args =
            [|
              ("const stack_t __user *", "uss"); ("stack_t __user *", "uoss");
            |];
        }
  | Signal ->
      Syscall
        {
          name = "signal";
          args = [| ("int", "sig"); ("__sighandler_t", "handler") |];
        }
  | Signalfd ->
      Syscall
        {
          name = "signalfd";
          args =
            [|
              ("int", "ufd");
              ("sigset_t __user *", "user_mask");
              ("size_t", "sizemask");
            |];
        }
  | Signalfd4 ->
      Syscall
        {
          name = "signalfd4";
          args =
            [|
              ("int", "ufd");
              ("sigset_t __user *", "user_mask");
              ("size_t", "sizemask");
              ("int", "flags");
            |];
        }
  | Sigpending ->
      Syscall
        { name = "sigpending"; args = [| ("old_sigset_t __user *", "uset") |] }
  | Sigprocmask ->
      Syscall
        {
          name = "sigprocmask";
          args =
            [|
              ("int", "how");
              ("old_sigset_t __user *", "nset");
              ("old_sigset_t __user *", "oset");
            |];
        }
  | Sigreturn -> Syscall { name = "sigreturn"; args = [||] }
  | Sigsuspend ->
      Syscall
        { name = "sigsuspend"; args = [| ("sigset_t __user *", "uset") |] }
  | Socket ->
      Syscall
        {
          name = "socket";
          args = [| ("int", "family"); ("int", "type"); ("int", "protocol") |];
        }
  | Socketcall ->
      Syscall
        {
          name = "socketcall";
          args = [| ("int", "call"); ("unsigned long __user *", "args") |];
        }
  | Socketpair ->
      Syscall
        {
          name = "socketpair";
          args =
            [|
              ("int", "family");
              ("int", "type");
              ("int", "protocol");
              ("int __user *", "usockvec");
            |];
        }
  | Spill -> Syscall { name = "spill"; args = [||] }
  | Splice ->
      Syscall
        {
          name = "splice";
          args =
            [|
              ("int", "fd_in");
              ("loff_t __user *", "off_in");
              ("int", "fd_out");
              ("loff_t __user *", "off_out");
              ("size_t", "len");
              ("unsigned int", "flags");
            |];
        }
  | Spu_create ->
      Syscall
        {
          name = "spu_create";
          args =
            [|
              ("const char __user *", "name");
              ("unsigned int", "flags");
              ("umode_t", "mode");
              ("int", "neighbor_fd");
            |];
        }
  | Spu_run ->
      Syscall
        {
          name = "spu_run";
          args =
            [|
              ("int", "fd");
              ("__u32 __user *", "unpc");
              ("__u32 __user *", "ustatus");
            |];
        }
  | Ssetmask -> Syscall { name = "ssetmask"; args = [| ("int", "newmask") |] }
  | Stat ->
      Syscall
        {
          name = "stat";
          args =
            [|
              ("const char __user *", "filename");
              ("struct __old_kernel_stat __user *", "statbuf");
            |];
        }
  | Stat64 ->
      Syscall
        {
          name = "stat64";
          args =
            [|
              ("const char __user *", "filename");
              ("struct stat64 __user *", "statbuf");
            |];
        }
  | Statfs ->
      Syscall
        {
          name = "statfs";
          args =
            [|
              ("const char __user *", "pathname");
              ("struct statfs __user *", "buf");
            |];
        }
  | Statfs64 ->
      Syscall
        {
          name = "statfs64";
          args =
            [|
              ("const char __user *", "pathname");
              ("size_t", "sz");
              ("struct statfs64 __user *", "buf");
            |];
        }
  | Statx ->
      Syscall
        {
          name = "statx";
          args =
            [|
              ("int", "dfd");
              ("const char __user *", "filename");
              ("unsigned", "flags");
              ("unsigned int", "mask");
              ("struct statx __user *", "buffer");
            |];
        }
  | Stime ->
      Syscall
        {
          name = "stime";
          args = [| ("__kernel_old_time_t __user *", "tptr") |];
        }
  | Stty -> Syscall { name = "stty"; args = [||] }
  | Subpage_prot ->
      Syscall
        {
          name = "subpage_prot";
          args =
            [|
              ("unsigned long", "addr");
              ("unsigned long", "len");
              ("u32 __user *", "map");
            |];
        }
  | Swapcontext ->
      Syscall
        {
          name = "swapcontext";
          args =
            [|
              ("struct ucontext __user *", "old_ctx");
              ("struct ucontext __user *", "new_ctx");
              ("long", "ctx_size");
            |];
        }
  | Swapoff ->
      Syscall
        {
          name = "swapoff";
          args = [| ("const char __user *", "specialfile") |];
        }
  | Swapon ->
      Syscall
        {
          name = "swapon";
          args =
            [| ("const char __user *", "specialfile"); ("int", "swap_flags") |];
        }
  | Switch_endian -> Syscall { name = "switch_endian"; args = [||] }
  | Symlink ->
      Syscall
        {
          name = "symlink";
          args =
            [|
              ("const char __user *", "oldname");
              ("const char __user *", "newname");
            |];
        }
  | Symlinkat ->
      Syscall
        {
          name = "symlinkat";
          args =
            [|
              ("const char __user *", "oldname");
              ("int", "newdfd");
              ("const char __user *", "newname");
            |];
        }
  | Sync -> Syscall { name = "sync"; args = [||] }
  | Sync_file_range ->
      Syscall
        {
          name = "sync_file_range";
          args =
            [|
              ("int", "fd");
              ("loff_t", "offset");
              ("loff_t", "nbytes");
              ("unsigned int", "flags");
            |];
        }
  | Sync_file_range2 ->
      Syscall
        {
          name = "sync_file_range2";
          args =
            [|
              ("int", "fd");
              ("unsigned int", "flags");
              ("loff_t", "offset");
              ("loff_t", "nbytes");
            |];
        }
  | Syncfs -> Syscall { name = "syncfs"; args = [| ("int", "fd") |] }
  | Sys_debug_setcontext ->
      Syscall { name = "sys_debug_setcontext"; args = [||] }
  | Syscall -> Syscall { name = "syscall"; args = [||] }
  | Sysfs ->
      Syscall
        {
          name = "sysfs";
          args =
            [|
              ("int", "option");
              ("unsigned long", "arg1");
              ("unsigned long", "arg2");
            |];
        }
  | Sysinfo ->
      Syscall
        { name = "sysinfo"; args = [| ("struct sysinfo __user *", "info") |] }
  | Syslog ->
      Syscall
        {
          name = "syslog";
          args = [| ("int", "type"); ("char __user *", "buf"); ("int", "len") |];
        }
  | Tee ->
      Syscall
        {
          name = "tee";
          args =
            [|
              ("int", "fdin");
              ("int", "fdout");
              ("size_t", "len");
              ("unsigned int", "flags");
            |];
        }
  | Tgkill ->
      Syscall
        {
          name = "tgkill";
          args = [| ("pid_t", "tgid"); ("pid_t", "pid"); ("int", "sig") |];
        }
  | Time ->
      Syscall
        { name = "time"; args = [| ("__kernel_old_time_t __user *", "tloc") |] }
  | Timer_create ->
      Syscall
        {
          name = "timer_create";
          args =
            [|
              ("const clockid_t", "which_clock");
              ("struct sigevent __user *", "timer_event_spec");
              ("timer_t __user *", "created_timer_id");
            |];
        }
  | Timer_delete ->
      Syscall { name = "timer_delete"; args = [| ("timer_t", "timer_id") |] }
  | Timer_getoverrun ->
      Syscall
        { name = "timer_getoverrun"; args = [| ("timer_t", "timer_id") |] }
  | Timer_gettime ->
      Syscall
        {
          name = "timer_gettime";
          args =
            [|
              ("timer_t", "timer_id");
              ("struct __kernel_itimerspec __user *", "setting");
            |];
        }
  | Timer_gettime64 -> Alias ("timer_gettime64", Timer_gettime)
  | Timer_settime ->
      Syscall
        {
          name = "timer_settime";
          args =
            [|
              ("timer_t", "timer_id");
              ("int", "flags");
              ("const struct __kernel_itimerspec __user *", "new_setting");
              ("struct __kernel_itimerspec __user *", "old_setting");
            |];
        }
  | Timer_settime64 -> Alias ("timer_settime64", Timer_settime)
  | Timerfd -> Syscall { name = "timerfd"; args = [||] }
  | Timerfd_create ->
      Syscall
        {
          name = "timerfd_create";
          args = [| ("int", "clockid"); ("int", "flags") |];
        }
  | Timerfd_gettime ->
      Syscall
        {
          name = "timerfd_gettime";
          args =
            [|
              ("int", "ufd"); ("struct __kernel_itimerspec __user *", "otmr");
            |];
        }
  | Timerfd_gettime64 -> Alias ("timerfd_gettime64", Timerfd_gettime)
  | Timerfd_settime ->
      Syscall
        {
          name = "timerfd_settime";
          args =
            [|
              ("int", "ufd");
              ("int", "flags");
              ("const struct __kernel_itimerspec __user *", "utmr");
              ("struct __kernel_itimerspec __user *", "otmr");
            |];
        }
  | Timerfd_settime64 -> Alias ("timerfd_settime64", Timerfd_settime)
  | Times ->
      Syscall { name = "times"; args = [| ("struct tms __user *", "tbuf") |] }
  | Tkill ->
      Syscall { name = "tkill"; args = [| ("pid_t", "pid"); ("int", "sig") |] }
  | Truncate ->
      Syscall
        {
          name = "truncate";
          args = [| ("const char __user *", "path"); ("long", "length") |];
        }
  | Truncate64 ->
      Syscall
        {
          name = "truncate64";
          args = [| ("const char __user *", "path"); ("loff_t", "length") |];
        }
  | Tuxcall -> Syscall { name = "tuxcall"; args = [||] }
  | Ugetrlimit -> Alias ("ugetrlimit", Getrlimit)
  | Ulimit -> Syscall { name = "ulimit"; args = [||] }
  | Umask -> Syscall { name = "umask"; args = [| ("int", "mask") |] }
  | Umount ->
      Syscall
        {
          name = "umount";
          args = [| ("char __user *", "name"); ("int", "flags") |];
        }
  | Umount2 -> Alias ("umount2", Umount)
  | Uname ->
      Syscall
        { name = "uname"; args = [| ("struct old_utsname __user *", "name") |] }
  | Unlink ->
      Syscall
        { name = "unlink"; args = [| ("const char __user *", "pathname") |] }
  | Unlinkat ->
      Syscall
        {
          name = "unlinkat";
          args =
            [|
              ("int", "dfd");
              ("const char __user *", "pathname");
              ("int", "flag");
            |];
        }
  | Unshare ->
      Syscall
        { name = "unshare"; args = [| ("unsigned long", "unshare_flags") |] }
  | Uselib ->
      Syscall
        { name = "uselib"; args = [| ("const char __user *", "library") |] }
  | Userfaultfd ->
      Syscall { name = "userfaultfd"; args = [| ("int", "flags") |] }
  | Ustat ->
      Syscall
        {
          name = "ustat";
          args = [| ("unsigned", "dev"); ("struct ustat __user *", "ubuf") |];
        }
  | Utime ->
      Syscall
        {
          name = "utime";
          args =
            [|
              ("char __user *", "filename"); ("struct utimbuf __user *", "times");
            |];
        }
  | Utimensat ->
      Syscall
        {
          name = "utimensat";
          args =
            [|
              ("int", "dfd");
              ("const char __user *", "filename");
              ("struct __kernel_timespec __user *", "utimes");
              ("int", "flags");
            |];
        }
  | Utimensat_time64 -> Alias ("utimensat_time64", Utimensat)
  | Utimes ->
      Syscall
        {
          name = "utimes";
          args =
            [|
              ("char __user *", "filename");
              ("struct __kernel_old_timeval __user *", "utimes");
            |];
        }
  | Utrap_install ->
      Syscall
        {
          name = "utrap_install";
          args =
            [|
              ("utrap_entry_t", "type");
              ("utrap_handler_t", "new_p");
              ("utrap_handler_t", "new_d");
              ("utrap_handler_t __user *", "old_p");
              ("utrap_handler_t __user *", "old_d");
            |];
        }
  | Vfork -> Syscall { name = "vfork"; args = [||] }
  | Vhangup -> Syscall { name = "vhangup"; args = [||] }
  | Vm86 ->
      Syscall
        {
          name = "vm86";
          args = [| ("unsigned long", "cmd"); ("unsigned long", "arg") |];
        }
  | Vm86old ->
      Syscall
        {
          name = "vm86old";
          args = [| ("struct vm86_struct __user *", "user_vm86") |];
        }
  | Vmsplice ->
      Syscall
        {
          name = "vmsplice";
          args =
            [|
              ("int", "fd");
              ("const struct iovec __user *", "uiov");
              ("unsigned long", "nr_segs");
              ("unsigned int", "flags");
            |];
        }
  | Vserver -> Syscall { name = "vserver"; args = [||] }
  | Wait4 ->
      Syscall
        {
          name = "wait4";
          args =
            [|
              ("pid_t", "upid");
              ("int __user *", "stat_addr");
              ("int", "options");
              ("struct rusage __user *", "ru");
            |];
        }
  | Waitid ->
      Syscall
        {
          name = "waitid";
          args =
            [|
              ("int", "which");
              ("pid_t", "upid");
              ("struct siginfo __user *", "infop");
              ("int", "options");
              ("struct rusage __user *", "ru");
            |];
        }
  | Waitpid ->
      Syscall
        {
          name = "waitpid";
          args =
            [|
              ("pid_t", "pid"); ("int __user *", "stat_addr"); ("int", "options");
            |];
        }
  | Write ->
      Syscall
        {
          name = "write";
          args =
            [|
              ("unsigned int", "fd");
              ("const char __user *", "buf");
              ("size_t", "count");
            |];
        }
  | Writev ->
      Syscall
        {
          name = "writev";
          args =
            [|
              ("unsigned long", "fd");
              ("const struct iovec __user *", "vec");
              ("unsigned long", "vlen");
            |];
        }
  | Xtensa -> Syscall { name = "xtensa"; args = [||] }

let name t =
  match description t with Syscall { name; _ } | Alias (name, _) -> name

let rec args t =
  match description t with
  | Syscall { args; _ } -> args
  | Alias (_, t) -> args t

type call =
  | Exit of t * Bitvector.t
  | Full of t * Bitvector.t array * Bitvector.t * (int * Loader_buf.t) array
  | Incomplete of t * Bitvector.t array * Bitvector.t
  | Unknown of int * Bitvector.t
  | Related of libcall

and libcall =
  | Clock_gettime of {
      clk_id : Bitvector.t;
      tp : Bitvector.t;
      res : Bitvector.t;
      tv_sec : Bitvector.t;
      tv_nsec : Bitvector.t;
    }

let pp_libcall ppf = function
  | Clock_gettime { clk_id; tp; res; tv_sec; tv_nsec } ->
      Format.fprintf ppf
        "clock_gettime(%a, %a) = %a { tv_sec = %a; tv_nsec = %a }" Bitvector.pp
        clk_id Bitvector.pp tp Bitvector.pp res Bitvector.pp tv_sec Bitvector.pp
        tv_nsec

let pp =
  let pp_aux ppf t args ret writes =
    Format.pp_print_string ppf (name t);
    Format.pp_print_char ppf '(';
    let n = Array.length args in
    if n > 0 then (
      Bitvector.pp_hex_or_bin ppf (Array.get args 0);
      for i = 1 to n - 1 do
        Format.pp_print_string ppf ", ";
        Bitvector.pp_hex_or_bin ppf (Array.get args i)
      done);
    Format.pp_print_string ppf ") = ";
    Bitvector.pp_hex_or_bin ppf ret;
    match t with
    | Read | Recvfrom ->
        let _, bytes = Array.get writes 0 in
        let len = Bigarray.Array1.dim bytes in
        if len < 100 then (
          Format.pp_print_string ppf " \"";
          for i = 0 to len - 1 do
            Format.pp_print_char ppf
              (Char.unsafe_chr (Bigarray.Array1.unsafe_get bytes i))
          done;
          Format.pp_print_char ppf '"')
    | _ -> ()
  in
  fun ppf t ->
    match t with
    | Unknown (i, ret) ->
        Format.fprintf ppf "[%d](??) = %a" i Bitvector.pp_hex_or_bin ret
    | Exit (t, status) ->
        Format.fprintf ppf "%s(%d)" (name t)
          (Bitvector.to_int (Bitvector.extract status { hi = 7; lo = 0 }))
    | Incomplete (t, args, ret) ->
        pp_aux ppf t args ret [||];
        Format.pp_print_string ppf " (unsupported)"
    | Full (t, args, ret, writes) -> pp_aux ppf t args ret writes
    | Related p -> pp_libcall ppf p

let exit t read cursor =
  let arg = read cursor in
  for _ = 1 to 6 - 1 do
    ignore (read cursor)
  done;
  Exit (t, arg)

let common t read cursor =
  let args = args t in
  let n = Array.length args in
  let args = Array.init n (fun _ -> read cursor) in
  for _ = n to 6 - 1 do
    ignore (read cursor)
  done;
  let ret = read cursor in
  (t, args, ret)

let simple t read cursor =
  let t, args, ret = common t read cursor in
  assert (Bitvector.is_zeros (read cursor));
  Full (t, args, ret, [||])

let out_data1 t i read cursor =
  let t, args, ret = common t read cursor in
  let size = Bitvector.to_uint (read cursor) in
  let { Loader_buf.buffer = data; _ } = Loader_buf.sub cursor size in
  Full (t, args, ret, [| (i, data) |])

let ioctl t read cursor =
  let t, args, ret = common t read cursor in
  let size = read cursor in
  match Bitvector.to_uint (Array.get args 1) with
  | 0x541B ->
      let { Loader_buf.buffer = data; _ } =
        Loader_buf.sub cursor (Bitvector.to_uint size)
      in
      Full (t, args, ret, [| (2, data) |])
  | _ ->
      assert (Bitvector.is_zeros size);
      Full (t, args, ret, [||])

let unsupported t read cursor =
  let t, args, ret = common t read cursor in
  let size = read cursor in
  ignore (Loader_buf.Read.bytes cursor (Bitvector.to_uint size));
  Incomplete (t, args, ret)

let unknown n read cursor =
  let ret = read cursor in
  Unknown (n, ret)

let reader decode i =
  try
    let t : t = decode i in
    match t with
    | Exit | Exit_group -> exit t
    | Read | Recvfrom -> out_data1 t 1
    | Getrandom -> out_data1 t 0
    | Write | Open | Close | Lseek | Mprotect | Munmap | Brk | Getpid | Socket
    | Connect | Fcntl | Getuid | Geteuid | Openat | Openat2 | Futex | Setsockopt
    | Sendto ->
        simple t
    | Mmap | Mmap2 -> out_data1 t (-1)
    | Ioctl -> ioctl t
    | Poll -> out_data1 t 0
    | Fstat -> out_data1 t 1
    | Rt_sigaction | Newfstatat -> out_data1 t 2
    | Statx -> out_data1 t 4
    | S_llseek -> out_data1 t 3
    | Sysinfo -> out_data1 t 0
    | _ -> unsupported t
  with Not_found -> unknown i

module X86_32 = struct
  let decode = function
    | 0 -> Restart_syscall
    | 1 -> Exit
    | 2 -> Fork
    | 3 -> Read
    | 4 -> Write
    | 5 -> Open
    | 6 -> Close
    | 7 -> Waitpid
    | 8 -> Creat
    | 9 -> Link
    | 10 -> Unlink
    | 11 -> Execve
    | 12 -> Chdir
    | 13 -> Time
    | 14 -> Mknod
    | 15 -> Chmod
    | 16 -> Lchown
    | 17 -> Break
    | 18 -> Oldstat
    | 19 -> Lseek
    | 20 -> Getpid
    | 21 -> Mount
    | 22 -> Umount
    | 23 -> Setuid
    | 24 -> Getuid
    | 25 -> Stime
    | 26 -> Ptrace
    | 27 -> Alarm
    | 28 -> Oldfstat
    | 29 -> Pause
    | 30 -> Utime
    | 31 -> Stty
    | 32 -> Gtty
    | 33 -> Access
    | 34 -> Nice
    | 35 -> Ftime
    | 36 -> Sync
    | 37 -> Kill
    | 38 -> Rename
    | 39 -> Mkdir
    | 40 -> Rmdir
    | 41 -> Dup
    | 42 -> Pipe
    | 43 -> Times
    | 44 -> Prof
    | 45 -> Brk
    | 46 -> Setgid
    | 47 -> Getgid
    | 48 -> Signal
    | 49 -> Geteuid
    | 50 -> Getegid
    | 51 -> Acct
    | 52 -> Umount2
    | 53 -> Lock
    | 54 -> Ioctl
    | 55 -> Fcntl
    | 56 -> Mpx
    | 57 -> Setpgid
    | 58 -> Ulimit
    | 59 -> Oldolduname
    | 60 -> Umask
    | 61 -> Chroot
    | 62 -> Ustat
    | 63 -> Dup2
    | 64 -> Getppid
    | 65 -> Getpgrp
    | 66 -> Setsid
    | 67 -> Sigaction
    | 68 -> Sgetmask
    | 69 -> Ssetmask
    | 70 -> Setreuid
    | 71 -> Setregid
    | 72 -> Sigsuspend
    | 73 -> Sigpending
    | 74 -> Sethostname
    | 75 -> Setrlimit
    | 76 -> Getrlimit
    | 77 -> Getrusage
    | 78 -> Gettimeofday
    | 79 -> Settimeofday
    | 80 -> Getgroups
    | 81 -> Setgroups
    | 82 -> Select
    | 83 -> Symlink
    | 84 -> Oldlstat
    | 85 -> Readlink
    | 86 -> Uselib
    | 87 -> Swapon
    | 88 -> Reboot
    | 89 -> Readdir
    | 90 -> Mmap
    | 91 -> Munmap
    | 92 -> Truncate
    | 93 -> Ftruncate
    | 94 -> Fchmod
    | 95 -> Fchown
    | 96 -> Getpriority
    | 97 -> Setpriority
    | 98 -> Profil
    | 99 -> Statfs
    | 100 -> Fstatfs
    | 101 -> Ioperm
    | 102 -> Socketcall
    | 103 -> Syslog
    | 104 -> Setitimer
    | 105 -> Getitimer
    | 106 -> Stat
    | 107 -> Lstat
    | 108 -> Fstat
    | 109 -> Olduname
    | 110 -> Iopl
    | 111 -> Vhangup
    | 112 -> Idle
    | 113 -> Vm86old
    | 114 -> Wait4
    | 115 -> Swapoff
    | 116 -> Sysinfo
    | 117 -> Ipc
    | 118 -> Fsync
    | 119 -> Sigreturn
    | 120 -> Clone
    | 121 -> Setdomainname
    | 122 -> Uname
    | 123 -> Modify_ldt
    | 124 -> Adjtimex
    | 125 -> Mprotect
    | 126 -> Sigprocmask
    | 127 -> Create_module
    | 128 -> Init_module
    | 129 -> Delete_module
    | 130 -> Get_kernel_syms
    | 131 -> Quotactl
    | 132 -> Getpgid
    | 133 -> Fchdir
    | 134 -> Bdflush
    | 135 -> Sysfs
    | 136 -> Personality
    | 137 -> Afs_syscall
    | 138 -> Setfsuid
    | 139 -> Setfsgid
    | 140 -> S_llseek
    | 141 -> Getdents
    | 142 -> S_newselect
    | 143 -> Flock
    | 144 -> Msync
    | 145 -> Readv
    | 146 -> Writev
    | 147 -> Getsid
    | 148 -> Fdatasync
    | 149 -> S_sysctl
    | 150 -> Mlock
    | 151 -> Munlock
    | 152 -> Mlockall
    | 153 -> Munlockall
    | 154 -> Sched_setparam
    | 155 -> Sched_getparam
    | 156 -> Sched_setscheduler
    | 157 -> Sched_getscheduler
    | 158 -> Sched_yield
    | 159 -> Sched_get_priority_max
    | 160 -> Sched_get_priority_min
    | 161 -> Sched_rr_get_interval
    | 162 -> Nanosleep
    | 163 -> Mremap
    | 164 -> Setresuid
    | 165 -> Getresuid
    | 166 -> Vm86
    | 167 -> Query_module
    | 168 -> Poll
    | 169 -> Nfsservctl
    | 170 -> Setresgid
    | 171 -> Getresgid
    | 172 -> Prctl
    | 173 -> Rt_sigreturn
    | 174 -> Rt_sigaction
    | 175 -> Rt_sigprocmask
    | 176 -> Rt_sigpending
    | 177 -> Rt_sigtimedwait
    | 178 -> Rt_sigqueueinfo
    | 179 -> Rt_sigsuspend
    | 180 -> Pread64
    | 181 -> Pwrite64
    | 182 -> Chown
    | 183 -> Getcwd
    | 184 -> Capget
    | 185 -> Capset
    | 186 -> Sigaltstack
    | 187 -> Sendfile
    | 188 -> Getpmsg
    | 189 -> Putpmsg
    | 190 -> Vfork
    | 191 -> Ugetrlimit
    | 192 -> Mmap2
    | 193 -> Truncate64
    | 194 -> Ftruncate64
    | 195 -> Stat64
    | 196 -> Lstat64
    | 197 -> Fstat64
    | 198 -> Lchown32
    | 199 -> Getuid32
    | 200 -> Getgid32
    | 201 -> Geteuid32
    | 202 -> Getegid32
    | 203 -> Setreuid32
    | 204 -> Setregid32
    | 205 -> Getgroups32
    | 206 -> Setgroups32
    | 207 -> Fchown32
    | 208 -> Setresuid32
    | 209 -> Getresuid32
    | 210 -> Setresgid32
    | 211 -> Getresgid32
    | 212 -> Chown32
    | 213 -> Setuid32
    | 214 -> Setgid32
    | 215 -> Setfsuid32
    | 216 -> Setfsgid32
    | 217 -> Pivot_root
    | 218 -> Mincore
    | 219 -> Madvise
    | 220 -> Getdents64
    | 221 -> Fcntl64
    | 224 -> Gettid
    | 225 -> Readahead
    | 226 -> Setxattr
    | 227 -> Lsetxattr
    | 228 -> Fsetxattr
    | 229 -> Getxattr
    | 230 -> Lgetxattr
    | 231 -> Fgetxattr
    | 232 -> Listxattr
    | 233 -> Llistxattr
    | 234 -> Flistxattr
    | 235 -> Removexattr
    | 236 -> Lremovexattr
    | 237 -> Fremovexattr
    | 238 -> Tkill
    | 239 -> Sendfile64
    | 240 -> Futex
    | 241 -> Sched_setaffinity
    | 242 -> Sched_getaffinity
    | 243 -> Set_thread_area
    | 244 -> Get_thread_area
    | 245 -> Io_setup
    | 246 -> Io_destroy
    | 247 -> Io_getevents
    | 248 -> Io_submit
    | 249 -> Io_cancel
    | 250 -> Fadvise64
    | 252 -> Exit_group
    | 253 -> Lookup_dcookie
    | 254 -> Epoll_create
    | 255 -> Epoll_ctl
    | 256 -> Epoll_wait
    | 257 -> Remap_file_pages
    | 258 -> Set_tid_address
    | 259 -> Timer_create
    | 260 -> Timer_settime
    | 261 -> Timer_gettime
    | 262 -> Timer_getoverrun
    | 263 -> Timer_delete
    | 264 -> Clock_settime
    | 265 -> Clock_gettime
    | 266 -> Clock_getres
    | 267 -> Clock_nanosleep
    | 268 -> Statfs64
    | 269 -> Fstatfs64
    | 270 -> Tgkill
    | 271 -> Utimes
    | 272 -> Fadvise64_64
    | 273 -> Vserver
    | 274 -> Mbind
    | 275 -> Get_mempolicy
    | 276 -> Set_mempolicy
    | 277 -> Mq_open
    | 278 -> Mq_unlink
    | 279 -> Mq_timedsend
    | 280 -> Mq_timedreceive
    | 281 -> Mq_notify
    | 282 -> Mq_getsetattr
    | 283 -> Kexec_load
    | 284 -> Waitid
    | 286 -> Add_key
    | 287 -> Request_key
    | 288 -> Keyctl
    | 289 -> Ioprio_set
    | 290 -> Ioprio_get
    | 291 -> Inotify_init
    | 292 -> Inotify_add_watch
    | 293 -> Inotify_rm_watch
    | 294 -> Migrate_pages
    | 295 -> Openat
    | 296 -> Mkdirat
    | 297 -> Mknodat
    | 298 -> Fchownat
    | 299 -> Futimesat
    | 300 -> Fstatat64
    | 301 -> Unlinkat
    | 302 -> Renameat
    | 303 -> Linkat
    | 304 -> Symlinkat
    | 305 -> Readlinkat
    | 306 -> Fchmodat
    | 307 -> Faccessat
    | 308 -> Pselect6
    | 309 -> Ppoll
    | 310 -> Unshare
    | 311 -> Set_robust_list
    | 312 -> Get_robust_list
    | 313 -> Splice
    | 314 -> Sync_file_range
    | 315 -> Tee
    | 316 -> Vmsplice
    | 317 -> Move_pages
    | 318 -> Getcpu
    | 319 -> Epoll_pwait
    | 320 -> Utimensat
    | 321 -> Signalfd
    | 322 -> Timerfd_create
    | 323 -> Eventfd
    | 324 -> Fallocate
    | 325 -> Timerfd_settime
    | 326 -> Timerfd_gettime
    | 327 -> Signalfd4
    | 328 -> Eventfd2
    | 329 -> Epoll_create1
    | 330 -> Dup3
    | 331 -> Pipe2
    | 332 -> Inotify_init1
    | 333 -> Preadv
    | 334 -> Pwritev
    | 335 -> Rt_tgsigqueueinfo
    | 336 -> Perf_event_open
    | 337 -> Recvmmsg
    | 338 -> Fanotify_init
    | 339 -> Fanotify_mark
    | 340 -> Prlimit64
    | 341 -> Name_to_handle_at
    | 342 -> Open_by_handle_at
    | 343 -> Clock_adjtime
    | 344 -> Syncfs
    | 345 -> Sendmmsg
    | 346 -> Setns
    | 347 -> Process_vm_readv
    | 348 -> Process_vm_writev
    | 349 -> Kcmp
    | 350 -> Finit_module
    | 351 -> Sched_setattr
    | 352 -> Sched_getattr
    | 353 -> Renameat2
    | 354 -> Seccomp
    | 355 -> Getrandom
    | 356 -> Memfd_create
    | 357 -> Bpf
    | 358 -> Execveat
    | 359 -> Socket
    | 360 -> Socketpair
    | 361 -> Bind
    | 362 -> Connect
    | 363 -> Listen
    | 364 -> Accept4
    | 365 -> Getsockopt
    | 366 -> Setsockopt
    | 367 -> Getsockname
    | 368 -> Getpeername
    | 369 -> Sendto
    | 370 -> Sendmsg
    | 371 -> Recvfrom
    | 372 -> Recvmsg
    | 373 -> Shutdown
    | 374 -> Userfaultfd
    | 375 -> Membarrier
    | 376 -> Mlock2
    | 377 -> Copy_file_range
    | 378 -> Preadv2
    | 379 -> Pwritev2
    | 380 -> Pkey_mprotect
    | 381 -> Pkey_alloc
    | 382 -> Pkey_free
    | 383 -> Statx
    | 384 -> Arch_prctl
    | 385 -> Io_pgetevents
    | 386 -> Rseq
    | 393 -> Semget
    | 394 -> Semctl
    | 395 -> Shmget
    | 396 -> Shmctl
    | 397 -> Shmat
    | 398 -> Shmdt
    | 399 -> Msgget
    | 400 -> Msgsnd
    | 401 -> Msgrcv
    | 402 -> Msgctl
    | 403 -> Clock_gettime64
    | 404 -> Clock_settime64
    | 405 -> Clock_adjtime64
    | 406 -> Clock_getres_time64
    | 407 -> Clock_nanosleep_time64
    | 408 -> Timer_gettime64
    | 409 -> Timer_settime64
    | 410 -> Timerfd_gettime64
    | 411 -> Timerfd_settime64
    | 412 -> Utimensat_time64
    | 413 -> Pselect6_time64
    | 414 -> Ppoll_time64
    | 416 -> Io_pgetevents_time64
    | 417 -> Recvmmsg_time64
    | 418 -> Mq_timedsend_time64
    | 419 -> Mq_timedreceive_time64
    | 420 -> Semtimedop_time64
    | 421 -> Rt_sigtimedwait_time64
    | 422 -> Futex_time64
    | 423 -> Sched_rr_get_interval_time64
    | 424 -> Pidfd_send_signal
    | 425 -> Io_uring_setup
    | 426 -> Io_uring_enter
    | 427 -> Io_uring_register
    | 428 -> Open_tree
    | 429 -> Move_mount
    | 430 -> Fsopen
    | 431 -> Fsconfig
    | 432 -> Fsmount
    | 433 -> Fspick
    | 434 -> Pidfd_open
    | 435 -> Clone3
    | 437 -> Openat2
    | 438 -> Pidfd_getfd
    | _ -> raise_notrace Not_found

  let tbl = Array.init 439 (reader decode)

  let read cursor = Bitvector.of_bits (Loader_buf.Read.bytes cursor 4)

  let read cursor =
    let n = Loader_buf.Read.s32 cursor in
    if n < 0 then (
      assert (n = -1);
      assert (Loader_buf.Read.u64 cursor = 4 * 5);
      let clk_id = read cursor in
      let tp = read cursor in
      let res = read cursor in
      let tv_sec = read cursor in
      let tv_nsec = read cursor in
      Related (Clock_gettime { clk_id; tp; res; tv_sec; tv_nsec }))
    else
      try (Array.get tbl n) read cursor
      with Invalid_argument _ -> unknown n read cursor
end

module X86_64 = struct
  let decode : int -> t = function
    | 0 -> Read
    | 1 -> Write
    | 2 -> Open
    | 3 -> Close
    | 4 -> Stat
    | 5 -> Fstat
    | 6 -> Lstat
    | 7 -> Poll
    | 8 -> Lseek
    | 9 -> Mmap
    | 10 -> Mprotect
    | 11 -> Munmap
    | 12 -> Brk
    | 13 -> Rt_sigaction
    | 14 -> Rt_sigprocmask
    | 15 -> Rt_sigreturn
    | 16 -> Ioctl
    | 17 -> Pread64
    | 18 -> Pwrite64
    | 19 -> Readv
    | 20 -> Writev
    | 21 -> Access
    | 22 -> Pipe
    | 23 -> Select
    | 24 -> Sched_yield
    | 25 -> Mremap
    | 26 -> Msync
    | 27 -> Mincore
    | 28 -> Madvise
    | 29 -> Shmget
    | 30 -> Shmat
    | 31 -> Shmctl
    | 32 -> Dup
    | 33 -> Dup2
    | 34 -> Pause
    | 35 -> Nanosleep
    | 36 -> Getitimer
    | 37 -> Alarm
    | 38 -> Setitimer
    | 39 -> Getpid
    | 40 -> Sendfile
    | 41 -> Socket
    | 42 -> Connect
    | 43 -> Accept
    | 44 -> Sendto
    | 45 -> Recvfrom
    | 46 -> Sendmsg
    | 47 -> Recvmsg
    | 48 -> Shutdown
    | 49 -> Bind
    | 50 -> Listen
    | 51 -> Getsockname
    | 52 -> Getpeername
    | 53 -> Socketpair
    | 54 -> Setsockopt
    | 55 -> Getsockopt
    | 56 -> Clone
    | 57 -> Fork
    | 58 -> Vfork
    | 59 -> Execve
    | 60 -> Exit
    | 61 -> Wait4
    | 62 -> Kill
    | 63 -> Uname
    | 64 -> Semget
    | 65 -> Semop
    | 66 -> Semctl
    | 67 -> Shmdt
    | 68 -> Msgget
    | 69 -> Msgsnd
    | 70 -> Msgrcv
    | 71 -> Msgctl
    | 72 -> Fcntl
    | 73 -> Flock
    | 74 -> Fsync
    | 75 -> Fdatasync
    | 76 -> Truncate
    | 77 -> Ftruncate
    | 78 -> Getdents
    | 79 -> Getcwd
    | 80 -> Chdir
    | 81 -> Fchdir
    | 82 -> Rename
    | 83 -> Mkdir
    | 84 -> Rmdir
    | 85 -> Creat
    | 86 -> Link
    | 87 -> Unlink
    | 88 -> Symlink
    | 89 -> Readlink
    | 90 -> Chmod
    | 91 -> Fchmod
    | 92 -> Chown
    | 93 -> Fchown
    | 94 -> Lchown
    | 95 -> Umask
    | 96 -> Gettimeofday
    | 97 -> Getrlimit
    | 98 -> Getrusage
    | 99 -> Sysinfo
    | 100 -> Times
    | 101 -> Ptrace
    | 102 -> Getuid
    | 103 -> Syslog
    | 104 -> Getgid
    | 105 -> Setuid
    | 106 -> Setgid
    | 107 -> Geteuid
    | 108 -> Getegid
    | 109 -> Setpgid
    | 110 -> Getppid
    | 111 -> Getpgrp
    | 112 -> Setsid
    | 113 -> Setreuid
    | 114 -> Setregid
    | 115 -> Getgroups
    | 116 -> Setgroups
    | 117 -> Setresuid
    | 118 -> Getresuid
    | 119 -> Setresgid
    | 120 -> Getresgid
    | 121 -> Getpgid
    | 122 -> Setfsuid
    | 123 -> Setfsgid
    | 124 -> Getsid
    | 125 -> Capget
    | 126 -> Capset
    | 127 -> Rt_sigpending
    | 128 -> Rt_sigtimedwait
    | 129 -> Rt_sigqueueinfo
    | 130 -> Rt_sigsuspend
    | 131 -> Sigaltstack
    | 132 -> Utime
    | 133 -> Mknod
    | 134 -> Uselib
    | 135 -> Personality
    | 136 -> Ustat
    | 137 -> Statfs
    | 138 -> Fstatfs
    | 139 -> Sysfs
    | 140 -> Getpriority
    | 141 -> Setpriority
    | 142 -> Sched_setparam
    | 143 -> Sched_getparam
    | 144 -> Sched_setscheduler
    | 145 -> Sched_getscheduler
    | 146 -> Sched_get_priority_max
    | 147 -> Sched_get_priority_min
    | 148 -> Sched_rr_get_interval
    | 149 -> Mlock
    | 150 -> Munlock
    | 151 -> Mlockall
    | 152 -> Munlockall
    | 153 -> Vhangup
    | 154 -> Modify_ldt
    | 155 -> Pivot_root
    | 156 -> S_sysctl
    | 157 -> Prctl
    | 158 -> Arch_prctl
    | 159 -> Adjtimex
    | 160 -> Setrlimit
    | 161 -> Chroot
    | 162 -> Sync
    | 163 -> Acct
    | 164 -> Settimeofday
    | 165 -> Mount
    | 166 -> Umount2
    | 167 -> Swapon
    | 168 -> Swapoff
    | 169 -> Reboot
    | 170 -> Sethostname
    | 171 -> Setdomainname
    | 172 -> Iopl
    | 173 -> Ioperm
    | 174 -> Create_module
    | 175 -> Init_module
    | 176 -> Delete_module
    | 177 -> Get_kernel_syms
    | 178 -> Query_module
    | 179 -> Quotactl
    | 180 -> Nfsservctl
    | 181 -> Getpmsg
    | 182 -> Putpmsg
    | 183 -> Afs_syscall
    | 184 -> Tuxcall
    | 186 -> Gettid
    | 187 -> Readahead
    | 188 -> Setxattr
    | 189 -> Lsetxattr
    | 190 -> Fsetxattr
    | 191 -> Getxattr
    | 192 -> Lgetxattr
    | 193 -> Fgetxattr
    | 194 -> Listxattr
    | 195 -> Llistxattr
    | 196 -> Flistxattr
    | 197 -> Removexattr
    | 198 -> Lremovexattr
    | 199 -> Fremovexattr
    | 200 -> Tkill
    | 201 -> Time
    | 202 -> Futex
    | 203 -> Sched_setaffinity
    | 204 -> Sched_getaffinity
    | 205 -> Set_thread_area
    | 206 -> Io_setup
    | 207 -> Io_destroy
    | 208 -> Io_getevents
    | 209 -> Io_submit
    | 210 -> Io_cancel
    | 211 -> Get_thread_area
    | 212 -> Lookup_dcookie
    | 213 -> Epoll_create
    | 216 -> Remap_file_pages
    | 217 -> Getdents64
    | 218 -> Set_tid_address
    | 219 -> Restart_syscall
    | 220 -> Semtimedop
    | 221 -> Fadvise64
    | 222 -> Timer_create
    | 223 -> Timer_settime
    | 224 -> Timer_gettime
    | 225 -> Timer_getoverrun
    | 226 -> Timer_delete
    | 227 -> Clock_settime
    | 228 -> Clock_gettime
    | 229 -> Clock_getres
    | 230 -> Clock_nanosleep
    | 231 -> Exit_group
    | 232 -> Epoll_wait
    | 233 -> Epoll_ctl
    | 234 -> Tgkill
    | 235 -> Utimes
    | 236 -> Vserver
    | 237 -> Mbind
    | 238 -> Set_mempolicy
    | 239 -> Get_mempolicy
    | 240 -> Mq_open
    | 241 -> Mq_unlink
    | 242 -> Mq_timedsend
    | 243 -> Mq_timedreceive
    | 244 -> Mq_notify
    | 245 -> Mq_getsetattr
    | 246 -> Kexec_load
    | 247 -> Waitid
    | 248 -> Add_key
    | 249 -> Request_key
    | 250 -> Keyctl
    | 251 -> Ioprio_set
    | 252 -> Ioprio_get
    | 253 -> Inotify_init
    | 254 -> Inotify_add_watch
    | 255 -> Inotify_rm_watch
    | 256 -> Migrate_pages
    | 257 -> Openat
    | 258 -> Mkdirat
    | 259 -> Mknodat
    | 260 -> Fchownat
    | 261 -> Futimesat
    | 262 -> Newfstatat
    | 263 -> Unlinkat
    | 264 -> Renameat
    | 265 -> Linkat
    | 266 -> Symlinkat
    | 267 -> Readlinkat
    | 268 -> Fchmodat
    | 269 -> Faccessat
    | 270 -> Pselect6
    | 271 -> Ppoll
    | 272 -> Unshare
    | 273 -> Set_robust_list
    | 274 -> Get_robust_list
    | 275 -> Splice
    | 276 -> Tee
    | 277 -> Sync_file_range
    | 278 -> Vmsplice
    | 279 -> Move_pages
    | 280 -> Utimensat
    | 281 -> Epoll_pwait
    | 282 -> Signalfd
    | 283 -> Timerfd_create
    | 284 -> Eventfd
    | 285 -> Fallocate
    | 286 -> Timerfd_settime
    | 287 -> Timerfd_gettime
    | 288 -> Accept4
    | 289 -> Signalfd4
    | 290 -> Eventfd2
    | 291 -> Epoll_create1
    | 292 -> Dup3
    | 293 -> Pipe2
    | 294 -> Inotify_init1
    | 295 -> Preadv
    | 296 -> Pwritev
    | 297 -> Rt_tgsigqueueinfo
    | 298 -> Perf_event_open
    | 299 -> Recvmmsg
    | 300 -> Fanotify_init
    | 301 -> Fanotify_mark
    | 302 -> Prlimit64
    | 303 -> Name_to_handle_at
    | 304 -> Open_by_handle_at
    | 305 -> Clock_adjtime
    | 306 -> Syncfs
    | 307 -> Sendmmsg
    | 308 -> Setns
    | 309 -> Getcpu
    | 310 -> Process_vm_readv
    | 311 -> Process_vm_writev
    | 312 -> Kcmp
    | 313 -> Finit_module
    | 314 -> Sched_setattr
    | 315 -> Sched_getattr
    | 316 -> Renameat2
    | 317 -> Seccomp
    | 318 -> Getrandom
    | 319 -> Memfd_create
    | 320 -> Kexec_file_load
    | 321 -> Bpf
    | 322 -> Execveat
    | 323 -> Userfaultfd
    | 324 -> Membarrier
    | 325 -> Mlock2
    | 326 -> Copy_file_range
    | 327 -> Preadv2
    | 328 -> Pwritev2
    | 329 -> Pkey_mprotect
    | 330 -> Pkey_alloc
    | 331 -> Pkey_free
    | 332 -> Statx
    | 333 -> Io_pgetevents
    | 334 -> Rseq
    | 424 -> Pidfd_send_signal
    | 425 -> Io_uring_setup
    | 426 -> Io_uring_enter
    | 427 -> Io_uring_register
    | 428 -> Open_tree
    | 429 -> Move_mount
    | 430 -> Fsopen
    | 431 -> Fsconfig
    | 432 -> Fsmount
    | 433 -> Fspick
    | 434 -> Pidfd_open
    | 435 -> Clone3
    | 437 -> Openat2
    | 438 -> Pidfd_getfd
    | _ -> raise_notrace Not_found

  let tbl = Array.init 439 (reader decode)

  let read cursor = Bitvector.of_bits (Loader_buf.Read.bytes cursor 8)

  let read cursor =
    let n = Loader_buf.Read.s64 cursor in
    if n < 0 then (
      assert (n = -1);
      assert (Loader_buf.Read.u64 cursor = 8 * 5);
      let clk_id = read cursor in
      let tp = read cursor in
      let res = read cursor in
      let tv_sec = read cursor in
      let tv_nsec = read cursor in
      Related (Clock_gettime { clk_id; tp; res; tv_sec; tv_nsec }))
    else
      try (Array.get tbl n) read cursor
      with Invalid_argument _ -> unknown n read cursor
end

let import =
  let rec loop read cursor trace =
    if Loader_buf.at_end cursor then trace
    else loop read cursor (read cursor :: trace)
  in

  fun ?(isa = Kernel_options.Machine.isa ()) path ->
    let read =
      match isa with
      | X86 { bits = `x32 } -> X86_32.read
      | X86 { bits = `x64 } -> X86_64.read
      | _ ->
          raise
            (Errors.not_yet_implemented "incomplete architecture definition")
    in
    let fd = Unix.openfile path [ Unix.O_RDONLY ] 0 in
    let buf =
      Bigarray.(
        array1_of_genarray
          (Unix.map_file fd Int8_unsigned C_layout false [| -1 |]))
    in
    let cursor = Loader_buf.cursor Machine.LittleEndian buf in
    List.rev (loop read cursor [])
