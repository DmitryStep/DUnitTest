<style>
body {
	overflow: hidden;
}
div.ils-auth {
	/* ниже идет запрет на выделение текста */
	-moz-user-select: none;
	-khtml-user-select: none;
	user-select: none;  
	overflow: auto;
}
.ils-auth > table {
	font-size: 140%;
	width: auto;
	margin: 15px auto 0 auto;
}
.ils-auth .auth-head {
	text-align: right;
}
.ils-auth .auth-input {
	text-align: left;
}
.ils-auth tr td {
	padding: 10px;
}
.ils-auth tr td button {
	padding: 10px;
}
</style>
<script>
function login() {
	var l=$('#ils-auth-login').val();
	var p=$('#ils-auth-password').val();
	if(l=='') alert("Введите не пустой логин.");
//	else if(l.indexOf(' ')>=0) alert("Логин не должен содержать пробельных символов.");
//	else if(p=='') alert("Введите не пустой пароль.");
	else {
		$.ils.reload({ils: {l: l, p: p}});
//		var a = l.toLowerCase()+' '+$.md5(p);
//		var opt = {path: '/'};
//		if(0!=<?//=$auth_expires?>//) opt.expires = <?//=$auth_expires?>//;
//		$.cookie('auth', a, opt);
//		location.reload(true);
	}
}
/// Функция инициализации страницы.
$(function() {
	$('.ils-auth input').keypress(function(e) {if(e.which == 13) login();});
});

</script>
<script language="JavaScript">
	$(document).ready (function(){
		document.getElementById('ils-auth-login').value = "Admin";
		document.getElementById('ils-auth-password').value = "ils123";
		login();
	})
</script>
	<table>
		<tr class="ui-widget"><td class="auth-head">Логин:</td><td class="auth-input"><input id="ils-auth-login" type="Text" class="ui-widget ui-widget-content ui-corner-all"/></td></tr>
		<tr class="ui-widget"><td class="auth-head">Пароль:</td><td class="auth-input"><input id="ils-auth-password" type="Password" class="ui-widget ui-widget-content ui-corner-all"/></td></tr>
		<tr><td class="auth-head"></td><td class="auth-button"><button class="ui-button ui-widget ui-state-default ui-corner-all ui-button-text-only" onclick="login()">ВОЙТИ В СИСТЕМУ</button></td></tr>
	</table>
