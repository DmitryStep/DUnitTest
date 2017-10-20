<?php

// functions

// Read from file
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

// Request form
function FormRequest($URL, $Request, $params){
}

// Parse JSON
function ParseJSONResponce(){
}

// Send request and Get response in JSON
function SendRequestAndGetResponse($request){
}

// Assert test results
function AssertResults(){
}

// Output to log in NUnit-format
function OutputToXMLLog($output_file){
}

// ---------------------------- End of functions -------------------------------

// Main Program

$URL = "http://test.ils-glonass.ru";

$tests = readtestfile("tests.txt");

foreach ($tests as $test) {
  echo $test."<br>";
}

?>
