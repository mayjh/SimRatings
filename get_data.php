<?php

$file_name = "data/".$_POST['file_name'];
$file=fopen($file_name,"r");
$read=fread($file,filesize($file_name));
fclose($file);
echo $read;

?>
