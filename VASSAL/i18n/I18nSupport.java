package VASSAL.i18n;

import java.io.IOException;
import java.io.OutputStream;

public class I18nSupport {

  static void writeProperty(OutputStream out, String name, String value) throws IOException {
    writeEscapedISO88591(out, name, TYPE_NAME);
    out.write('=');
    writeEscapedISO88591(out, value, TYPE_VALUE);
    out.write('\n');
  }
  
  protected static final int TYPE_COMMENT = 0;
  protected static final int TYPE_NAME = 1;
  protected static final int TYPE_VALUE = 2;

  static void writeEscapedISO88591(OutputStream out, String s, int type) throws IOException {
    for (int i=0; i<s.length(); i++){
      int c = (int)s.charAt(i);
      if (c < 0x100){
        boolean escape = false;
        if (c == '\r' || c == '\n' || c == '\\'){
          escape = true;
        } else if (c == ' ' || c == '\t' || c == '\f'){
          if(type == TYPE_NAME){
            escape = true;
          } else if (type == TYPE_VALUE && (i==0 || i == s.length() - 1)){
            escape = true;
          }
        } else if (type == TYPE_NAME && (c == '=' || c == ':')){
          escape = true;
        }
        if (escape){
          switch (c){
            case '\n': {
              switch (type){
                case TYPE_COMMENT: {
                  out.write('\n');
                  out.write('#');
                  out.write(' ');
                } break;
                case TYPE_NAME: {
                  out.write('\\');
                  out.write('n');
                  out.write('\\');
                  out.write('\n');
                  out.write('\t');
                } break;
                case TYPE_VALUE: {
                  out.write('\\');
                  out.write('n');
                  out.write('\\');
                  out.write('\n');
                  out.write('\t');
                  out.write('\t');
                } break;
              }
            } break;
            case '\\': {
              out.write('\\');
              out.write('\\');
            } break;
            case '\r': {
              out.write('\\');
              out.write('r');
            } break;
            case '\t': {
              out.write('\\');
              out.write('t');
            } break;
            case '\f': {
              out.write('\\');
              out.write('f');
            } break;
            default : {
              out.write('\\');
              out.write((byte)c);
            } break;
          }
        } else {
          out.write((byte)c);
        }
      } else {
        out.write('\\');
        out.write('u');
        out.write(prepad(Integer.toHexString(c), 4, '0').getBytes("ISO-8859-1"));
      }
    }
  }
  
  public static String prepad(String s, int length, char c){
    int needed = length - s.length();
    if (needed <= 0){
      return s;
    }
    char padding[] = new char[needed];
    java.util.Arrays.fill(padding, c);
    StringBuffer sb = new StringBuffer(length);
    sb.append(padding);
    sb.append(s);
    return sb.toString();
  }
  
}