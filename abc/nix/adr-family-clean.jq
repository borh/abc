def family_problem($pattern):
  (((.["claim-id"] // "") | test($pattern))
   or any((.["affected-claim-ids"] // [])[]; test($pattern))
   or ((.file // "") as $file
       | any(($pattern | scan("[0-9]{4}"));
             . as $number | $file | startswith($number))));
[.problems[] | select(family_problem($family))] | length == 0
