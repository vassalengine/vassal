/* 
 * $Id$
 *
 * Copyright (c) 2000-2007 by Rodney Kinney
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.chat;

import java.io.*;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Properties;
import java.util.StringTokenizer;

/**
 * Performs Get and Post operations to a given URL
 */
public class HttpRequestWrapper {
  private String baseURL;

  public HttpRequestWrapper(String baseURL) {
    this.baseURL = baseURL;
  }

  public Enumeration doGet(Properties p) throws IOException {
    return doGet("",p); //$NON-NLS-1$
  }

  /**
   * Perform a GET request
   * @param url the URL relative to the base URL
   * @param props additional query parameters
   * @return an Enumeration of Strings, one for each line in the response
   * @throws IOException
   */
  public Enumeration doGet(String url, Properties props) throws IOException {
    url = baseURL + url;
    if (props != null) {
       url += "?"; //$NON-NLS-1$
      for (Enumeration e = props.keys();
           e.hasMoreElements();) {
        String key = (String) e.nextElement();
        String value = props.getProperty(key);
        url += key + "=" + URLEncoder.encode(value,"UTF-8"); //$NON-NLS-1$ //$NON-NLS-2$
        if (e.hasMoreElements()) {
          url += "&"; //$NON-NLS-1$
        }
      }
    }
    URL base = new URL(url);
    URLConnection conn = base.openConnection();
    conn.setUseCaches(false);
    BufferedInputStream in = new BufferedInputStream(conn.getInputStream());
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    byte[] buffer = new byte[100000];
    int length;
    while ((length = in.read(buffer)) > 0) {
      out.write(buffer,0,length);
    }
    in.close();
    ArrayList<String> l = new ArrayList<String>();
    StringTokenizer st = new StringTokenizer(new String(out.toByteArray(),"UTF-8"),"\n\r"); //$NON-NLS-1$ //$NON-NLS-2$
    while (st.hasMoreTokens()) {
      l.add(st.nextToken());
    }
    return Collections.enumeration(l);
  }

  public Enumeration doPost(Properties p) throws IOException {
    return doPost("",p); //$NON-NLS-1$
  }

  public Enumeration doPost(String url, Properties props) throws IOException {
    url = baseURL + url;
    String content = ""; //$NON-NLS-1$
    if (props != null) {
      for (Enumeration e = props.keys();
           e.hasMoreElements();) {
        String key = (String) e.nextElement();
        String value = props.getProperty(key);
        content += key + "=" + URLEncoder.encode(value,"UTF-8"); //$NON-NLS-1$ //$NON-NLS-2$
        if (e.hasMoreElements()) {
          content += "&"; //$NON-NLS-1$
        }
      }
    }
    URL base = new URL(url);
    URLConnection conn = base.openConnection();
    conn.setDoInput(true);
    conn.setDoOutput(true);
    conn.setUseCaches(false);
    //	    conn.setRequestProperty("Content-Type","application/x-www-form-urlencoded");
    DataOutputStream out = new DataOutputStream(conn.getOutputStream());
    out.writeBytes(content);
    out.flush();
    out.close();
    BufferedReader input = new BufferedReader
      (new InputStreamReader(conn.getInputStream(),"UTF-8")); //$NON-NLS-1$
    ArrayList<String> l = new ArrayList<String>();
    for (String line = input.readLine();
         line != null; line = input.readLine()) {
      l.add(line);
    }
    input.close();
    return Collections.enumeration(l);
  }
}
