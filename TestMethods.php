<?php

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

?>