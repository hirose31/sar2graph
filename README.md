# sar2graph.pl

`sar2graph.pl` reads ouput of `sar -A` and writes several graphs using rrdtool.

## Usage

```
sar -A > sar.txt
sar2graph.pl -s '2015-09-03 21:00:00' -e '2015-09-03 23:50:00' -f sar.txt
```

