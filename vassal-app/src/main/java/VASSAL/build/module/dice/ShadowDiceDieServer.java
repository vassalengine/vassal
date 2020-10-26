
package VASSAL.build.module.dice;

import java.util.Enumeration;
import java.util.StringTokenizer;
import java.util.Vector;

import VASSAL.build.module.DieRoll;
import VASSAL.tools.FormattedString;

/**
 *
 * ShadowDice Dice Server
 *
 */
public class ShadowDiceDieServer extends DieServer {
  public static final String ROLL_MARKER = "VASSAL auto-generated dice roll";

  public ShadowDiceDieServer() {

    name = "ShadowDice";
    description = "ShadowDice Dice Server";
    emailOnly = false;
    maxRolls = 0;
    maxEmails = 0;
    serverURL = "http://www.gamerz.net/shadowdice/shadowdice.cgi";
    passwdRequired = false;
    canDoSeparateDice = true;
  }

  @Override
  public String[] buildInternetRollString(RollSet toss) {

    final String CRLF = "%0D%0A"; // CRLF
    final String LSQUARE = "%5B"; // '['
    final String RSQUARE = "%5D"; // ']'
    final String HASH = "%23";
//      final String PLUS = "%2B";

    final String desc;
    String s;
    String pEmail = "";
    String sEmail = "";

    if (getUseEmail()) {
      pEmail = extractEmail(getPrimaryEmail());
      sEmail = extractEmail(getSecondaryEmail());
    }

    desc = hexify(toss.description);

    s = "mto=" + pEmail + "&mcc=" + sEmail + "&yem=" + pEmail;
    s += "&sbj=" + desc;
    s += "&msg=" + ROLL_MARKER + CRLF + desc + CRLF;

    final int mLen = toss.getMaxDescLength();

    final DieRoll[] rolls = toss.getDieRolls();
    for (final DieRoll roll : rolls) {
      s += hexify(roll.getDescription());
      for (int j = 0; j < mLen - roll.getDescription().length(); j++) {
        s += ' ';
      }
      s += ' ' + HASH;
      final int nd = roll.getNumDice();
      final int ns = roll.getNumSides();
      for (int j = 0; j < nd; j++) {
        s += LSQUARE + "1d" + ns + RSQUARE;
      }
      s += CRLF;
    }

    s += "&todo=Action%21&hid=1";
    s = s.replace(' ', '+'); // No spaces allowed, use '+' instead.

    return new String[]{s};
  }

  /*
   * The Irony server requires most of the non-alphanumerics to be
   * converted to a hex escape code %nn. '*-_.' excepted.
   * '#' characters interfere with the output parsing and are stripped out.
   */
  public String hexify(String s) {

    final String hexyChars = "~!$%^&()+`={}[]|:;'<>,?/\\\"";
    final StringBuilder b = new StringBuilder();

    for (int i = 0; i < s.length(); i++) {
      final char c = s.charAt(i);

      if (c == '#') {
        b.append('.');
      }
      else if (hexyChars.indexOf(c) >= 0) {
        b.append('%').append(Integer.toHexString(c).toUpperCase());
      }
      else {
        b.append(c);
      }
    }
    return b.toString();
  }

  @Override
  public void parseInternetRollString(RollSet rollSet, Vector<String> results) {

    final Enumeration<String> e = results.elements();

    // Initialise and search for start line
    String line =  e.nextElement();
    while (e.hasMoreElements() && !line.startsWith("! " + ROLL_MARKER))
      line = e.nextElement();

    // Skip description line
    line = e.nextElement();

    // And process the results, 1 per roll in the multiroll
    final DieRoll[] rolls = rollSet.getDieRolls();
    for (int i = 0; i < rolls.length; i++) {

      line = e.nextElement();

      final int firsthash = line.indexOf('#') - 1;
      final StringTokenizer st = new StringTokenizer(line.substring(firsthash), " ");

      for (int j = 0; j < rollSet.dieRolls[i].getNumDice(); j++) {
        st.nextToken();
        final String result = st.nextToken();
        final int res = Integer.parseInt(result);
        rollSet.dieRolls[i].setResult(j, res);
      }
    }
  }

  @Override
  public void roll(RollSet mr, FormattedString format) {
    super.doInternetRoll(mr, format);
  }

}
