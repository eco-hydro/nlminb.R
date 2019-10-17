
*gdb or lldb 调试只支持linux和mac系统*

```r
R -d lldb

# void nlminb_iterate(double b[], double d[], double fx, double g[], double h[],
#            int iv[], int liv, int lv, int n, double v[], double x[])
b nlminb_iterate
```

## 调试

|----  | --------------------   | 
| print *x@2 | 从数组x中取出前两位元素| 
| print x    |                        |



## Julia

```bash
ps auxww | grep julia
gdb -p 1743

gdb --args julia nlminb_iterate
b nlminb_iterate

```


问题完美解决了。
- 如果需要传入空数组，需要设置b = C_NULL。
- gdb调试c，在linux或mac下，
    ```
    # 可以独立运行的jl file, C function在 jl file中调用
    $ gdb --args julia nlminb_iterate.jl

    # breakpoint function name
    (gdb) b nlminb_iterate

    # 显示所有的参数
    (gdb) info args

    # 显示数组值, *代表取值，@2代表前2个元素
    (gdb) print *b@2
    ```