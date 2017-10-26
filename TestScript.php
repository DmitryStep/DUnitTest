<?php

// -------------- Набор функций для обмена данными с сервером --------------

// Создаём соединение с уже пройденной авторизацией (нужный кукиш копируется в www/tmp перед стартом скрипта)
function create_Curl_Connect($Referer_URL){
  $Result = curl_init();
  curl_setopt($Result, CURLOPT_VERBOSE, True);
  curl_setopt($Result, CURLOPT_RETURNTRANSFER, True);
  curl_setopt($Result, CURLOPT_UNRESTRICTED_AUTH, True);
  curl_setopt($Result, CURLOPT_HEADER, False);
  curl_setopt($Result, CURLOPT_SSL_VERIFYPEER, false);
  curl_setopt($Result, CURLOPT_SSL_VERIFYHOST, false);
  curl_setopt($Result, CURLOPT_REFERER, $Referer_URL);
  return $Result;
}

// Посылаем запрос, на выходе получаем текст JSON
function SendRequest($Curl_Var, $Request_Str){
  curl_setopt($Curl_Var, CURLOPT_URL, $Request_Str);
  curl_setopt($Curl_Var, CURLOPT_COOKIE, "PHPSESSID=2m7ee1t6374qpol7bk3e1geb11"); // <-- а это тот самый кукишок с уже готовой авторизацией внутри
  $Result = curl_exec($Curl_Var);
  return $Result;
}

// Убийца соединения
function close_Curl_Connect($Curl_Var){
  curl_close($Curl_Var);
}

// ---------------------- Конец набора функций CURL -----------------

// ---------------------- Функции обработки данных ------------------

// Parse parameters
function GetParameters($param_str){
  $Result = explode("#", $param_str);
  return $Result;
}

// Forming of Request
function GetRequest($URL, $Request_str, $Parameters_str){
	$Params = GetParameters($Parameters_str);
	$Result = "";
	if (count($Params) > 1){
		$ValueIndex = 0;
		$Request = explode("&", $Request_str);
		foreach ($Request as $Request_param){
			$Result = $Result.$Request_param.$Params[$ValueIndex]."&";
			$ValueIndex++;
		}
		$Result = rtrim($Result, "&");
	}
	else{
		$Result = $Request_str;
	}
	$Result = $URL.$Result;
	return $Result;
}

// Read from file
function readtestfile($input_file){
  $Result = file_get_contents($input_file);
  return $Result;
}

// Parse source JSON-file with tests
function ParseJSONTests($JSONdata){
  $Result = JSON_decode($JSONdata);
  return $Result;
}

// Parse responce from server as array
function ParseJSONResponse($Response){
  $Result = JSON_decode($Response, true);
  return $Result;
}

// ------------------- Конец набора Функций обработки данных ------------------

// ------------------- Типа тестовый "движок" ---------------------------------

// Test failed
function Fail($TestCase, $FailMessage){
  $Result->testresult = 'failed';
  if ($FailMessage == ""){
    $Result->errorStackTrace = $TestCase->failmessage;
  }else{
    $Result->errorStackTrace = $FailMessage;
  }
  $Result->failedsince = 214;
  return $Result;
}

// Test passed
function Pass($TestCase){
  $Result->testresult = 'passed';
  $Result->errorStackTrace = "";
  $Result->failedsince = 0;
  return $Result;
}

// Error test
function Error($TestCase, $ErrorMessage){
  $Result->testresult = "error";
  $Result->errorStackTrace = $ErrorMessage;
  $Result->failedsince = 290;
  return $Result;
}

// Assert test results
function AssertTest($TestCase, $ActualResult){
  try{
    $Operation = $TestCase->operation;
    $ExpectedResult = $TestCase->expectedresult;
    if ($Operation == "="){
      $AssertionResult = ($ExpectedResult == $ActualResult);
    }
    if ($Operation == "!="){
      $AssertionResult = ($ExpectedResult != $ActualResult);
    }
    if ($Operation == ">="){
      $AssertionResult = ($ExpectedResult >= $ActualResult);
    }
    if ($Operation == "<="){
      $AssertionResult = ($ExpectedResult <= $ActualResult);
    }
    if ($Operation == ">"){
      $AssertionResult = ($ExpectedResult > $ActualResult);
    }
    if ($Operation == "<"){
      $AssertionResult = ($ExpectedResult < $ActualResult);
    }

    if (!$AssertionResult){
      $FailMessage = $TestCase->failmessage." Actual = '".$ActualResult."'. Expected = '".$TestCase->expectedresult."'. Operation: '".$TestCase->operation."'";
      $Result = Fail($TestCase, $FailMessage);
    }
    else{
      $Result = Pass($TestCase);
    }
  }catch(Exception $e){
    $Result = Error($TestCase, $e->GetMessage());
  }
  return $Result;
}

// Run single test from Suite
function RunTest($CurlVar, $URL, $Suite, $TestCase){
  $IsError = false;
  $time_beg = microtime(true);
  $Request = GetRequest($URL, $Suite->request, $TestCase->params);
  try{
    $Response = SendRequest($CurlVar, $Request);
    $JSON_Response = ParseJSONResponse($Response);
  }catch(Exception $e){
    $Result = Error($TestCase, $e->GetMessage());
    $IsError = true;
  }
  if (!$IsError){
    $Method = $TestCase->testmethod;
    if ($TestCase->methodparams != ""){
      $method_params = GetParameters($TestCase->methodparams);
      $ActualResult = $Method($JSON_Response, $method_params);
    }else{
      $ActualResult = $Method($JSON_Response);
    }
//    $ActualResult = 10;
    $Result->actual = $ActualResult;
    $Result->expected = $TestCase->expectedresult;
    $Result = AssertTest($TestCase, $ActualResult);
  }
  else
  {
    if ($Result->errorStackTrace == ""){
      $Result = Error($TestCase, "Error!");
    }
  }
  $Result->suitename = $Suite->name;
  $Result->testname = $TestCase->name;
  $time_end = microtime(true);
  $Result->duration = Round($time_end - $time_beg, 3);
  return $Result;
}

// Output Test Results into log in JUnit-format
function OutputToXMLLog($output_file, $TestSuitesArray, $TestResultsArray, $FullDuration){
  $XML_doc = new DomDocument("1.0", "UTF-8");
//  <result plugin="junit@1.21">
  $TestReport = $XML_doc->appendChild($XML_doc->createElement('result'));
  $Plugin = $XML_doc->CreateAttribute('plugin');
  $Plugin->value = "junit@1.21";
  $TestReport->appendChild($Plugin);
  $Suites = $TestReport->appendChild($XML_doc->createElement('suites'));

  foreach ($TestSuitesArray as $TestSuite){
    $SuiteDuration = 0;
    $Suite = $Suites->appendChild($XML_doc->createElement('suite'));
    $Name = $Suite->appendChild($XML_doc->createElement('name'));
    $Name->appendChild($XML_doc->createTextNode($TestSuite->name));

    $File = $Suite->appendChild($XML_doc->createElement('file'));
    $File->appendChild($XML_doc->createTextNode($TestSuite->name));

    $TestCases = $Suite->appendChild($XML_doc->createElement('cases'));

    foreach ($TestResultsArray as $TestResult){
      if ($TestResult->suitename == $TestSuite->name){
        $SuiteDuration = $SuiteDuration + $TestResult->duration;

        $TestCase = $TestCases->appendChild($XML_doc->createElement('case'));

        $Duration = $TestCase->appendChild($XML_doc->createElement('duration'));
        $Duration->appendChild($XML_doc->createTextNode($TestResult->duration));

        $ClassName = $TestCase->appendChild($XML_doc->createElement('ClassName'));
        $ClassName->appendChild($XML_doc->createTextNode($TestResult->suitename));

        $TestName = $TestCase->appendChild($XML_doc->createElement('testName'));
        $TestName->appendChild($XML_doc->createTextNode($TestResult->testname));

        $Skip = $TestCase->appendChild($XML_doc->createElement('skipped'));
        $Skip->appendChild($XML_doc->createTextNode('false'));

        $failedSince = $TestCase->appendChild($XML_doc->createElement('failedSince'));
        $failedSince->appendChild($XML_doc->createTextNode($TestResult->failedsince));

        if ($TestResult->errorStackTrace != ""){
          $ErrorStackTrace = $TestCase->appendChild($XML_doc->createElement('errorStackTrace'));
          $ErrorStackTrace->appendChild($XML_doc->createTextNode($TestResult->errorStackTrace));
        }
      }
    }

    $SuiteDur = $Suite->appendChild($XML_doc->createElement('duration'));
    $SuiteDur->appendChild($XML_doc->createTextNode(Round($SuiteDuration, 3)));
  }

  $XMLFullDuration = $TestReport->appendChild($XML_doc->createElement('duration'));
  $XMLFullDuration->appendChild($XML_doc->createTextNode($FullDuration));

  $LongStdio = $TestReport->appendChild($XML_doc->createElement('keepLongStdio'));
  $LongStdio->appendChild($XML_doc->createTextNode("true"));

  $XML_doc->formatOutput = true;
  $XML_doc->save($output_file);
}
// ------------------------ Конец тестового "движка" --------------------------

// ----------------------------- Тестовые методы ------------------------------

// Return count of array's elements
function GetCount($ElementsArray){
  $Result = Count($ElementsArray);
  return $Result;
}

// Return count of fields in the element of array
function GetFieldsCount($ElementsArray, $args){
  $Element = $ElementsArray[$args[0]];
  $Result = Count($Element);
  return $Result;
}

// Return value of array's element
function GetValue($ElementsArray, $args){
  $Element = $ElementsArray[$args[0]];
  $Field = $Element[$args[1]];
  return $Field;
}

// ------------------------- Конец тестовых методов ---------------------------

// ---------------------------- Main Program ----------------------------------

$URL_str = "http://test.ils-glonass.ru";

$Full_time_beg = microtime(true);

$suites = ParseJSONTests(readtestfile("tests.txt"));

$ch = create_Curl_Connect($URL_str);
$TestIndex = 0;
foreach ($suites as $suite){
  $tests = $suite->Tests;
  foreach ($tests as $test) {
    $test_results[$TestIndex] = RunTest($ch, $URL_str, $suite, $test);
    $TestIndex++;
  }
}

close_Curl_Connect($ch);

$Full_time_end = microtime(true);

$FullDuration = Round($Full_time_end - $Full_time_beg, 3);

OutputToXMLLog("junitResult.xml", $suites, $test_results, $FullDuration);

?>
