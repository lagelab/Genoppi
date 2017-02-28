rm data/snp_to_gene.txt
{
  while read snp; do
    grep -w -h -m 1 -r $snp . | cut -f1 -f2 -f3 -f4 -f5 | while read -r snp chr bp start end; do
      awk -v snp="$snp" -v loc="$bp" -v ch="$chr" -v start="$start" -v end="$end" '{ if (($3 == ch && $1 >= start && $1 <= end && $2 >= start && $2 <= end) ||
      ($3 == ch && $1 <= start && $2 >= start && $2 <= end) || ($3 == ch && $1 >= start && $1 <= end && $2 >= end) ||
      ($3 == ch && $1 <= start && $2 >= end)) print snp, $3, loc, $4, start, end}' OFS="\t" data/ensembl_homo_sapiens_genes.txt >> data/snp_to_gene.txt
    done
  done
  echo "snpid\thg19chrc\tbp\tgene\tld_m50kb\tld_p50kb" | cat - data/snp_to_gene.txt > /tmp/out && mv /tmp/out data/snp_to_gene.txt
} < data/snp.txt
