/*
 *
 * Copyright (c) 2007 Joel Uckelman
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

package VASSAL.build.module.dice;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Iterator;
import java.util.StringTokenizer;
import java.util.Vector;

import VASSAL.build.module.DieRoll;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.FormattedString;

public class BonesDiceServer extends DieServer {

  public BonesDiceServer() {
    name = "Bones";
    description = "Bones Dice Server";
    emailOnly = false;
    maxRolls = 0;
    maxEmails = 0;
    serverURL = "http://dice.nomic.net/cgi-bin/randroll.pl";
    passwdRequired = false;
    canDoSeparateDice = true;
  }

  @Override
  public String[] buildInternetRollString(RollSet toss) {
    DieRoll[] rolls = toss.getDieRolls();
    StringBuilder query = new StringBuilder("req=");

    // format is "{{ xdy + n }}"
    for (DieRoll roll : rolls) {
      query.append("{{")
           .append(roll.getNumDice())
           .append("D")
           .append(roll.getNumSides());

      if (roll.getPlus() != 0) {
        query.append("+").append(roll.getPlus());
      }

      query.append("}}\n");
    }

    try {
      return new String[] { new URI("http",
                          "dice.nomic.net",
                          "/cgi-bin/randroll.pl",
                          query.toString(),
                          null).toURL().toString() };

//      return new String[] {  URLEncoder.encode(query.toString(), "UTF-8") };
    }
    catch (MalformedURLException | URISyntaxException e) {
      // should never happen
      ErrorDialog.bug(e);
    }

    return null;
  }

  @Override
  public void parseInternetRollString(RollSet rollSet, Vector<String> results) {
    final Iterator<String> line = results.iterator();

    for (int i = 0; i < rollSet.dieRolls.length; ++i) {
      final StringTokenizer st = new StringTokenizer(line.next(), " ");

      for (int j = 0; j < rollSet.dieRolls[i].getNumDice(); ++j) {
        rollSet.dieRolls[i].setResult(j, Integer.parseInt(st.nextToken()));
      }
    }
  }

  @Override
  public void roll(RollSet mr, FormattedString format) {
    super.doInternetRoll(mr, format);
  }

  @Override
  public void doIRoll(RollSet toss) throws IOException {
    final String[] rollString = buildInternetRollString(toss);
    final Vector<String> returnString = new Vector<>();

    final URL url = new URL(rollString[0]);
    final HttpURLConnection connection =
      (HttpURLConnection) url.openConnection();
    connection.setRequestMethod("GET");
    connection.connect();

    try (BufferedReader in = new BufferedReader(
      new InputStreamReader(connection.getInputStream()))) {

      String line;
      while ((line = in.readLine()) != null)
        returnString.add(line);
    }

    parseInternetRollString(toss, returnString);
  }
}
