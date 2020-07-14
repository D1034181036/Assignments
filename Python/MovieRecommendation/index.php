<?php
	define("CONFIG",json_decode(file_get_contents("./config.json"), true)); //取得CONFIG
	$db_con = mysqli_connect(CONFIG['DB_HOST'],CONFIG['DB_USER'],CONFIG['DB_PASSWORD'],CONFIG['DB_TABLE']) or die('資料庫連線失敗'); //連接資料庫
	mysqli_query($db_con, 'SET NAMES utf8');
?>

<!DOCTYPE html>
<html>
<head>  
    <title>Movie</title>
    <!-- 輸入提示js -->
    <link rel="stylesheet" href="./js/jquery-ui.css" />
    <script src="./js/jquery-1.11.1.js"></script>
    <script src="./js/jquery-ui.js"></script>
	<script type="text/javascript" src="./js/auto_complete.js"></script>
</head>
<body>

<!-- 標題 -->
<h1 align="center">Movie Recommender System (Demo)</h1>

<!-- 輸入電影名稱欄 -->
<form id="form1" method="get" action="movie.php" align="center">	
	<input id="auto_autocomplete" name="movieName" type="text"  style="font-size:16px" placeholder="請輸入電影名稱"/>
	<input type="submit"  style="font-size:16px" value="查詢">
</form></br>


<!-- 預設電影顯示 -->
<?php
	$sql = "SELECT * FROM `movies` limit 20"; 
	$result = mysqli_query($db_con, $sql);
?>
<table border="1" align="center">
	<!-- table title -->
	<tr>
	    <th>movieName</th>
	    <th>movieName</th>
	</tr>

	<!-- table content -->
	<?php
		$i=0;
		while($value = mysqli_fetch_assoc($result)){
			if($i%2==0){echo "<tr>";}
			echo "<td>";
			echo "<a href='movie.php?movieId=$value[movieId]'>$value[title]</a>";
			echo "</td>";
			if($i%2==1){echo "</tr>";}
			$i++;
		}
	?>
</table>

</body>
</html>