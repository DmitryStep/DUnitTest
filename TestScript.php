<?php

function readtestfile($input_file){
  $tests_array = "";
  $testfile = fopen($input_file, "r");
  $Index = 0;
  while (!feof($testfile)){
    $tests_array[$Index] = fgets($testfile);
    $Index++;
  }
  fclose($testfile);
  return $tests_array;
}

function FormRequest($URL, $Request, $params){
}

function SendRequestAndGetResponse($request){
}

function ParseJSONResponce(){
}

function AssertResults(){
}

function OutputToXMLLog($output_file){
}

$tests = readtestfile("tests.txt");

foreach ($tests as $test) {
  echo $test."<br>";
}

?>
