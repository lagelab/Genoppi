#!/bin/sh
m_start=`cat data/ext_val.txt`
p_end=`cat data/ext_val.txt`
#echo "$m_start"
#echo "$p_end"
rm data/snp_to_gene.txt
{
  while read snp; do
    grep -w -h -m 1 -r $snp data/1kg/ | cut -f 1,2,3,4,5 | while read -r snp chr bp start end; do
      awk -v snp="$snp" -v loc="$bp" -v ch="$chr" -v start="$start" -v end="$end" -v m_start="$m_start" -v p_end="$p_end" '{ if (($3 == ch && $1 >= start-m_start && $1 <= end+p_end && $2 >= start-m_start && $2 <= end+p_end) ||
      ($3 == ch && $1 <= start-m_start && $2 >= start-m_start && $2 <= end+p_end) || ($3 == ch && $1 >= start-m_start && $1 <= end+p_end && $2 >= end+p_end) ||
      ($3 == ch && $1 <= start-m_start && $2 >= end+p_end)) print snp, $3, loc, $4, start-m_start, end+p_end}' OFS="\t" data/ensembl_homo_sapiens_genes.txt >> data/snp_to_gene.txt
    done
  done
  echo "snpid\thg19chrc\tbp\tgene\tld_m\tld_p" | cat - data/snp_to_gene.txt > /tmp/out && mv /tmp/out data/snp_to_gene.txt
} < data/snp.txt

if [ ! -f data/snp_to_gene.txt ]; then
    echo "File not found!"
    echo "snpid\thg19chrc\tbp\tgene\tld_m\tld_p" > data/snp_to_gene.txt
fi

rm data/ext_val.txt
