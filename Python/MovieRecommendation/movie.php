<?php
	define("CONFIG",json_decode(file_get_contents("./config.json"), true)); //取得CONFIG
	$db_con = mysqli_connect(CONFIG['DB_HOST'],CONFIG['DB_USER'],CONFIG['DB_PASSWORD'],CONFIG['DB_TABLE']) or die('資料庫連線失敗'); //連接資料庫
	mysqli_query($db_con, 'SET NAMES utf8');
?>

<!DOCTYPE html>
<html>
<head>  
    <title>Movie</title>
</head>
<body>

<!-- 標題 -->
<h1 align="center">Movie Recommender System (Demo)</h1>

<!-- 取得$movieId -->
<?php
	if(isset($_GET['movieName'])){
		$sql = "SELECT * FROM `movies` WHERE title = \"$_GET[movieName]\"";
		$result = mysqli_query($db_con, $sql);
		while($value = mysqli_fetch_assoc($result)){ 
	    	$movieId = $value['movieId'];
		}
	}else{
		$movieId = $_GET['movieId'];
	}
?>

<!-- 取得電影資訊 -->
<?php
	$sql = "SELECT * FROM `movies`,`links` WHERE movies.movieId=$movieId AND links.movieId=$movieId "; 
	$result = mysqli_query($db_con, $sql);
	while($value = mysqli_fetch_assoc($result)){ 
    	$movieName = $value['title'];
    	$movieLink = str_pad($value['imdbId'],7,'0',STR_PAD_LEFT);
	}
?>

<!-- 顯示電影資訊 -->
<h1 align="center"><a href="https://www.imdb.com/title/tt<?php echo $movieLink;?>/"><?php echo $movieName;?></a></h1>


<table border="1" align="center">
	<!-- table title -->
	<tr>
    	<th>movieId</th>
    	<th>movieName</th>
    	<th>similarity</th>
	</tr>
 
<!-- 取得推薦電影資訊 -->
<?php
	$get_similarity = json_decode(shell_exec(CONFIG['PYTHON_PATH'] . " get_similarity.py $movieId"));

	foreach ($get_similarity as $value) {

		//取得單部推薦電影資訊
		$sql = "SELECT * FROM `movies`,`links` WHERE movies.movieId=$value[0] AND links.movieId=$value[0] "; 
		$result = mysqli_query($db_con, $sql);
		while($v = mysqli_fetch_assoc($result)){ 
	    	$movieName = $v['title'];
	    	$movieLink = str_pad($v['imdbId'],7,'0',STR_PAD_LEFT);
		}
?>
	<!-- table content -->
	<tr>
		<td><?php echo $value[0];?></td>
		<td><a href="https://www.imdb.com/title/tt<?php echo $movieLink;?>/"> <?php echo $movieName;?></a></td>
		<td><?php echo round($value[1],3);?></td>
	</tr>

<?php
	} //foreach end
?>
</table>

<br><div align="center"><h2><a href="index.php">返回首頁</a></h2></div>
</body>
</html>