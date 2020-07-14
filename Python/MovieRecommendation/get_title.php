<?php
	define("CONFIG",json_decode(file_get_contents("./config.json"), true)); //取得CONFIG
	$db_con = mysqli_connect(CONFIG['DB_HOST'],CONFIG['DB_USER'],CONFIG['DB_PASSWORD'],CONFIG['DB_TABLE']) or die('資料庫連線失敗'); //連接資料庫
	mysqli_query($db_con, 'SET NAMES utf8');
?>

<?php
$sql ="SELECT * FROM `movies` WHERE `title` like '%" . $_GET['term'] . "%' limit 20";  
               
if ( $result = mysqli_query($db_con, $sql) ) { 
  while( $row = mysqli_fetch_array($result) ){ 
        $name[] = $row['title'];
   }     
	mysqli_free_result($result);
} 

mysqli_close($db_con);
echo json_encode($name); //輸出json
?>