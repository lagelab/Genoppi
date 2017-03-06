#!/bin/bash

TAB=`echo -e "\t"`

### t.txt is a text file with pasted lists of gene from Excel
tr '\t' '\n' <data/t.txt |awk 'NF'|sort -u> data/genes_to_add.txt

### find families for each gene from nonSGS
rm data/gene_families.txt
echo -e "gene"$'\t'"family" > data/gene_families.txt
for i in `cat data/genes_to_add.txt`
do
	#grep -P "^$i\t" data/hugo_families.csv >> data/gene_families.txt
	grep "^$i${TAB}" data/hugo_families.csv >> data/gene_families.txt
done

### find genes not listed in HUGO families
rm a data/genes_not_assigned_to_family.txt
for i in `cat data/genes_to_add.txt`
do
	echo -n $i "" >> a
	#grep -P -q "^$i\t" data/hugo_families.csv && echo 'string found' || echo 'string not found' >>a
	grep -q "^$i${TAB}" data/hugo_families.csv && echo 'string found' || echo 'string not found' >>a
	echo >> a
done
grep "string not found" a | awk '{print $1}' > data/genes_not_assigned_to_family.txt
rm a

### find list of families
#awk 'BEGIN {FS="\t"};{print $2}' families1.txt | sort -u > families2.txt

### prepare files for each family with nonSGS genes belonging to it
#for i in `cat families2.txt`
#do
#	grep -w $i families1.txt >> tmp
#	sort -u tmp | awk '{print $1}' > ${i}.txt
#	rm tmp
#done

### add full name to proteins that were not assigned to a family
rm data/genes_naf_wnames.txt
echo -e "gene"$'\t'"name" > data/genes_naf_wnames.txt
for i in `cat data/genes_not_assigned_to_family.txt`
do
    grep -w "^$i" data/UniProt-ID_gene_mapping_Apr142016.csv >> data/genes_naf_wnames.txt
done

rm data/genes_to_add.txt data/t.txt data/genes_not_assigned_to_family.txt
