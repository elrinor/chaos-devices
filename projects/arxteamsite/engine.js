var update = "24.01.04";
var piccolor="111111";
var adres="www.arx-team.narod.ru";

//===============================================================================================================================\\
// ������ ����
//===============================================================================================================================\\
function menu(x){
document.write("<TITLE>[ArX]team - "+ x +"</TITLE>"+
"  <link href='default.css' rel='stylesheet' type='text/css'>"+
"  </head>"+
"<BODY>"+
"<table align='center' cellspacing='0' cellpadding='0' border='0' width='790'>"+
"<tr>"+
"    <td background='border_uUL.gif' height='20' width='20'></td>"+
"    <td background='border_U.gif' height='20' width='120'></td>"+
"    <td background='border_uU.gif' height='20' width='20'></td>"+
"    <td background='border_U.gif' height='20' width='610'></td>"+
"    <td background='border_uUL.gif' style='filter:fliph()' height='20' width='20'></td>"+
"</tr>"+
"<tr height='180'>"+
"    <td background='border_L.gif' width='20'></td>"+
"    <td background='kirpichi.jpg' height='180' width='120' nowrap='1' valign='top'>"+
"     <img src='kirplogo.jpg' alt='' width='120' height='180' border='0'><br>"+
"     <font color='BBBBBB' class='menu'>"+
"     ����������:<br>"+
"	  <a href='index.html'>�������</a><br>"+
"     <a href='about.html'>� ��� � � �����</a><br>");

if (x=="� ���") {
  document.write(
  "<ul class='menu'>"+
  "<li><a href='aboutelric.html'>Elric! � ����</a></li>"+
  "<li><a href='aboutloky.html'>Loky � ����</a></li>"+
  "<li><a href='aboutzloy.html'>Zloy � ����</a></li>"+
  "</ul>");}

document.write(
"	  <br>"+
"	  ���� ��������� � �������:<br>"+
"	  <a href='q2.html'>Quake 2</a><br>");

if (x=="Quake 2"){
  document.write(
  "<ul class='menu'>"+
  "<li><a href='q2config.html'>��� Quake2 ������</a></li>"+
  "<li><a href='q2configcreation.html'>�������� ��������</a></li>"+
  "</ul>");}

document.write("	  <a href='rl.html'>Roguelike development</a><br>");

if (x=="Roguelike Development"){
  document.write(
  "<ul class='menu'>"+
  "<li><a href='rlarticles.html'>������</a></li>"+
  "</ul>");}

document.write(
"	  <br>"+
"	  ������:<br>"+
"	  <a href='Downloads.html'>Downloads</a><br>"+
"	  <a href='http://narod.yandex.ru/userforum/?owner=arx-team'>�����</a><br>"+
"     <br><br><br><br><br><br><br><br><br><br><br>"+
"	  </font>"+
"    </td>"+
"    <td background='border_M.gif' widht='20'></td>"+
"    <td width='610' valign='top'>");
}


//===============================================================================================================================\\
// ����� ����
//===============================================================================================================================\\
function endmenu(){
document.write(
"    </td>"+
"    <td background='border_L.gif' style='filter:fliph()' width='20'></td>"+
"</tr>"+
"<tr>"+
"    <td background='border_uDL.gif'  height='20' width='20'></td>"+
"    <td background='border_D.gif' height='20' <!--width='120'-->></td>"+
"    <td background='border_uD.gif' height='20' width='20'></td>"+
"    <td background='border_D.gif' height='20' <!--width='610'-->></td>"+
"    <td height='20' width='20'><img src='border_uDL.gif' style='filter:fliph()' alt='' width='20' height='20' border='0'></td>"+
"</tr>"+
"</table>");
}

//===============================================================================================================================\\
// �������� � ��������
//===============================================================================================================================\\
function picture(src, linkto, height, width, undertext, bgcolor, txtcolor, txtclass, align){
document.write("<TABLE cellPadding=5 align=", align ," border=0><TBODY><TR><TD><TABLE cellPadding=3 bgColor=", bgcolor ,"><TBODY><TR><TD><FONT><A href=", linkto ,"><IMG height=", height ," alt='' src=", src ," width=", width ," border=0></A></FONT></TD></TR><TR><TD><FONT color=", txtcolor ," class=",txtclass,">", undertext ,"</FONT></TD></TR></TBODY></TABLE></TD></TR></TBODY></TABLE>");
}

//===============================================================================================================================\\
// ���� �� ��� ����
//===============================================================================================================================\\
function homesite(){
document.write("<a href='http://",adres,"'>",adres,"</a>");
}

//===============================================================================================================================\\
// ������� <br> 
//===============================================================================================================================\\
function brr(q){
var i;
for (i = 1; i <= q; i++) {document.write("<br>")}
}




