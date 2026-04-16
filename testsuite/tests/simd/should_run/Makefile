TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T25030:
	'$(TEST_HC)' $(TEST_HC_OPTS) T25030.hs -v0 -O1 -fforce-recomp -ddump-cmm > T25030.cmm 2>&1

	# testFoldPlus: 111111+121212=232323, 121212+131313=252525 should be folded
	grep -m 1 -o "232323" T25030.cmm
	grep -m 1 -o "252525" T25030.cmm
	# operands should not appear in the output
	grep -o "111111" T25030.cmm || echo "Does not appear: 111111"
	grep -o "121212" T25030.cmm || echo "Does not appear: 121212"
	grep -o "131313" T25030.cmm || echo "Does not appear: 131313"

	# testFoldMax: max(333333,333332)=333333 should be folded
	grep -m 1 -o "333333" T25030.cmm
	# lesser operand should not appear
	grep -o "333332" T25030.cmm || echo "Does not appear: 333332"

	# testNeg: negate(343434)=-343434 should be folded
	grep -m 1 -o -- "-343434" T25030.cmm

	# testInserts: insert 363636 into broadcast(353535) and extract it;
	# should fold to constant 363636
	grep -m 1 -o "363636" T25030.cmm
	# broadcast operand should not appear
	grep -o "353535" T25030.cmm || echo "Does not appear: 353535"

	# testInserts2: 383838+393939=777777 should be folded
	grep -m 1 -o "777777" T25030.cmm
	# addends should not appear
	grep -o "383838" T25030.cmm || echo "Does not appear: 383838"

	# testOverwrite: inserting 404040,404041 into broadcast(414141) should fold to <404040,404041>
	grep -m 1 -o "404040" T25030.cmm
	grep -m 1 -o "404041" T25030.cmm
	# original broadcast value should not appear
	grep -o "414141" T25030.cmm || echo "Does not appear: 414141"

	# testExtractFromInsert: extract(insert(unknown_v, 454545, 3), 3) should fold to 454545
	grep -m 1 -o "454545" T25030.cmm
