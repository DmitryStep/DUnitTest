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
  $Result->testresult = 'Failure';
  if ($FailMessage == ""){
    $Result->errorStackTrace = $TestCase->failmessage;
  }else{
    $Result->errorStackTrace = $FailMessage;
  }
  return $Result;
}

// Test passed
function Pass($TestCase){
  $Result->testresult = 'Pass';
  return $Result;
}

// Error test
function Error($TestCase, $ErrorMessage){
  $Result->testresult = "Error";
  $Result->errorStackTrace = $ErrorMessage;
  return $Result;
}

// Assert test results
function AssertTest($TestCase, $ActualResult){
  try{
    $Operation = StrVal($TestCase->operation);
    $ExpectedResult = StrVal($TestCase->expectedresult);
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
    echo $Response;
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
function OutputToXMLLog($output_file, $TestSuitesArray, $TestResultsArray, $FullDuration, $StartDateTime, $FinishDateTime){
  $failures = 0;
  $passed = 0;
  $errors = 0;

  $XML_doc = new DomDocument("1.0", "UTF-8");
  $TestReport = $XML_doc->appendChild($XML_doc->createElement('test-results'));
  $Attr = $XML_doc->CreateAttribute('time');
  $Attr->value = Date("h:m:s");
  $TestReport->appendChild($Attr);
  $Attr = $XML_doc->CreateAttribute('date');
  $Attr->value = Date("d.m.Y");
  $TestReport->appendChild($Attr);
  $Attr = $XML_doc->CreateAttribute('notrun');
  $Attr->value = 0;
  $TestReport->appendChild($Attr);
  $Attr = $XML_doc->CreateAttribute('total');
  $Attr->value = Count($TestResultsArray);
  $TestReport->appendChild($Attr);

  foreach ($TestSuitesArray as $TestSuite){
    $TestsCount = 0;
    $SuiteDuration = 0;
    $Suite = $TestReport->appendChild($XML_doc->createElement('test-suite'));
    $Attr = $XML_doc->CreateAttribute('notrun');
    $Attr->value = 0;
    $Suite->appendChild($Attr);
    $Attr = $XML_doc->CreateAttribute('name');
    $Attr->value = $TestSuite->name;
    $Suite->appendChild($Attr);
    $Results = $Suite->appendChild($XML_doc->createElement('results'));

    foreach ($TestResultsArray as $TestResult){
      if ($TestResult->suitename == $TestSuite->name){
        $TestsCount++;
        $TestCase = $Results->appendChild($XML_doc->createElement('test-case'));
        $Attr = $XML_doc->CreateAttribute('time');
        $Attr->value = $TestResult->duration;
        $TestCase->appendChild($Attr);
        $Attr = $XML_doc->CreateAttribute('name');
        $Attr->value = $TestResult->testname;
        $TestCase->appendChild($Attr);
        $Attr = $XML_doc->CreateAttribute('result');
        $Attr->value = $TestResult->testresult;
        $TestCase->appendChild($Attr);
        $Attr = $XML_doc->CreateAttribute('executed');
        $Attr->value = "true";
        $TestCase->appendChild($Attr);
        if ($TestResult->testresult == "Pass"){
          $passed++;
          $Attr = $XML_doc->CreateAttribute('success');
          $Attr->value = "true";
          $TestCase->appendChild($Attr);
        }else
        if ($TestResult->testresult == "Failure"){
          $failures++;
          $Attr = $XML_doc->CreateAttribute('success');
          $Attr->value = "false";
          $TestCase->appendChild($Attr);
          $Fail = $TestCase->appendChild($XML_doc->createElement('failure'));
          $Msg = $Fail->appendChild($XML_doc->createElement('message'));
          $convertedText = mb_convert_encoding($TestResult->errorStackTrace, 'utf-8', mb_detect_encoding($TestResult->errorStackTrace));
          $Msg->appendChild($XML_doc->createTextNode($convertedText));
        }
        else{
          $errors++;
          $Attr = $XML_doc->CreateAttribute('success');
          $Attr->value = "false";
          $TestCase->appendChild($Attr);
          $Fail = $TestCase->appendChild($XML_doc->createElement('failure'));
          $Msg = $Fail->appendChild($XML_doc->createElement('message'));
          $convertedText = mb_convert_encoding($TestResult->errorStackTrace, 'utf-8', mb_detect_encoding($TestResult->errorStackTrace));
          $Msg->appendChild($XML_doc->createTextNode($convertedText));
        }
      }
    }
    $Attr = $XML_doc->CreateAttribute('total');
    $Attr->value = $TestsCount;
    $Suite->appendChild($Attr);
  }
  $FullStat = $TestReport->appendChild($XML_doc->createElement('statistics'));
  $Stat = $FullStat->appendChild($XML_doc->createElement('Stat'));
  $Attr = $XML_doc->CreateAttribute('tests');
  $Attr->value = Count($TestResultsArray);
  $Stat->appendChild($Attr);
  $Stat = $FullStat->appendChild($XML_doc->createElement('Stat'));
  $Attr = $XML_doc->CreateAttribute('failures');
  $Attr->value = $failures;
  $Stat->appendChild($Attr);
  $Stat = $FullStat->appendChild($XML_doc->createElement('Stat'));
  $Attr = $XML_doc->CreateAttribute('errors');
  $Attr->value = $errors;
  $Stat->appendChild($Attr);
  $Stat = $FullStat->appendChild($XML_doc->createElement('Stat'));
  $Attr = $XML_doc->CreateAttribute('success-rate');
  $Attr->value = Round($passed * 100 / Count($TestResultsArray), 2);
  $Stat->appendChild($Attr);
  $Stat = $FullStat->appendChild($XML_doc->createElement('Stat'));
  $Attr = $XML_doc->CreateAttribute('started_at');
  $Attr->value = $StartDateTime;
  $Stat->appendChild($Attr);
  $Stat = $FullStat->appendChild($XML_doc->createElement('Stat'));
  $Attr = $XML_doc->CreateAttribute('finished-at');
  $Attr->value = $FinishDateTime;
  $Stat->appendChild($Attr);
  $Stat = $FullStat->appendChild($XML_doc->createElement('Stat'));
  $Attr = $XML_doc->CreateAttribute('runtime');
  $Attr->value = $FullDuration;
  $Stat->appendChild($Attr);

  $XML_doc->formatOutput = true;
  $XML_doc->save($output_file);
}
// ------------------------ Конец тестового "движка" --------------------------

// ----------------------------- Тестовые методы ------------------------------

// Return count of array's elements
function GetCount($ElementsArray){
  $Body = $ElementsArray["Body"];
  $Element = $Body[0];
  $Items = $Element["Items"];
  $Result = Count($Items);
  return $Result;
}

// Return count of fields in the element of array
function GetFieldsCount($ElementsArray, $args){
  $Result = 0;
  $Body = $ElementsArray["Body"];
  $Element = $Body[0];
  $Items = $Element["Items"];
  foreach ($Items as $Item){
    if ($Item["id"]==$args[0]){
      $Result = Count($Item);
      break;
    }
  }
  return $Result;
}

// Return value of array's element
function GetValue($ElementsArray, $args){
  $Body = $ElementsArray["Body"];
  $Element = $Body[0];
  $Items = $Element["Items"];
  foreach ($Items as $Item){
    if ($Item["id"]==$args[0]){
      $Result = $Item[$args[1]];
      break;
    }
  }
  if ($Result == ""){
    $Result = "null";
  }
  return $Result;
}

// ------------------------- Конец тестовых методов ---------------------------

// ---------------------------- Main Program ----------------------------------

$URL_str = "http://test.ils-glonass.ru";

$StartDate = Date("d.m.Y h:m:s");
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
$FinishDate = Date("d.m.Y h:m:s");

OutputToXMLLog("PHP-Result.xml", $suites, $test_results, $FullDuration, $StartDate, $FinishDate);

?>
