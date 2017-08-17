<%-- 
    Document   : process
    Created on : Jul 13, 2017, 6:52:54 PM
    Author     : User
--%>


<%@ page import ="java.util.ArrayList" %>
<%@page import="java.text.SimpleDateFormat"%>
<%@page import="java.text.DateFormat"%>
<%@taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c"%> 
<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@page import="java.io.*"%>
<%@ page import ="java.sql.*" %>
<%@page import="org.apache.poi.hssf.usermodel.HSSFSheet"%>
<%@page import="org.apache.poi.hssf.usermodel.HSSFWorkbook"%>
<%@page import="org.apache.poi.hssf.usermodel.HSSFRow"%>
<%@page import="org.apache.poi.hssf.usermodel.HSSFCell"%>
<%@ page import ="java.util.Calendar" %>
<%@ page import ="rcaller.RCaller" %>
<%@ page import ="rcaller.RCode" %>
<% String a=null; String b=null;
    String value=request.getParameter("comment");
try
{
FileWriter fw = new FileWriter("E:\\xampp\\htdocs\\FYP\\web\\testing.csv");
       value= '"'+value+'"';
       fw.append("Tweet");
       fw.append('\n');
       fw.append(value);
       fw.append('\n');

fw.flush();
fw.close();

} catch (Exception e) {
e.printStackTrace();
}

%>

<%  
  RCaller caller = new RCaller(); 
        RCode code = new RCode(); 
code.addRCode("source('E:/xampp/htdocs/FYP/web/firststep.R')"); 
caller.setRCode(code); 
caller.setRscriptExecutable("D:/Program Files/R/R-3.4.1/bin/Rscript.exe");
  caller.runOnly(); 

ArrayList numbers = new ArrayList();
ArrayList numberss = new ArrayList();
    try
{
FileReader fw = new FileReader("E:\\xampp\\htdocs\\FYP\\web\\datac.csv");
BufferedReader br = new BufferedReader(fw); 
String s; String[] str2; 
while((s = br.readLine()) != null) { 
        str2 = s.split(",");
        numbers.add(str2[1]);
} 
                      fw.close();
                      } catch (Exception e) {
                      e.printStackTrace();
                      }
    %>
     <p style="background-color:#F0F8FF; margin-bottom: 0; width: 85%;padding-top: 10px; padding-bottom: 10px; padding-left: 20px;">  
             <%                    
                 out.print(value); %></p>
     <br>
     <p style="background-color:buttonface; margin-bottom: 0; width: 85%;padding-top: 10px; padding-bottom: 10px; padding-left: 20px;">  
           
     <%
out.print((String)numbers.get(1)); %>
     </p>
     <br>
     <%

try{

    		File file = new File("E:\\xampp\\htdocs\\FYP\\web\\datac.csv");

    		if(file.delete()){
    			//out.println(file.getName() + " is deleted!");
    		}else{
    			//out.println("Delete operation is failed.");
    		}

    	}catch(Exception e){

    		e.printStackTrace();

    	}
try{

    		File file = new File("E:\\xampp\\htdocs\\FYP\\web\\testing.csv");

    		if(file.delete()){
    			//out.println(file.getName() + " is deleted!");
    		}else{
    			//out.println("");
    		}

    	}catch(Exception e){

    		e.printStackTrace();

    	}
%>

<a href="index.html">Post New Query</a>