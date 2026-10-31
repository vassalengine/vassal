/*
 *
 * Copyright (c) 2026 by The VASSAL Development Team
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
package VASSAL.command;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.function.Function;

import org.junit.jupiter.api.Test;

import VASSAL.tools.SequenceEncoder;

/**
 * {@link CommandSerializer} must write exactly the text
 * {@code GameModule.encode(Command)} writes and read exactly the tree
 * {@code GameModule.decode(String)} reads, so those two algorithms are
 * reproduced here as references (they are private to the module and need a
 * live module) and every case is checked four ways: stream-written text
 * equals String-written text; stream-read tree equals String-read tree; and
 * each reader reads the other writer's text.
 */
public class CommandSerializerTest {
  private static final char DELIM = '\u001b';

  /** A command whose own text is a string, or null for a command no encoder knows. */
  static final class Text extends Command {
    final String text;

    Text(String text, Command... subs) {
      this.text = text;
      for (final Command s : subs) {
        append(s);
      }
    }

    @Override
    protected void executeCommand() {
    }

    @Override
    protected Command myUndoCommand() {
      return null;
    }
  }

  /** Command.append() drops null commands; a shape string makes trees comparable. */
  private static String shape(Command c) {
    if (c == null) {
      return "<null>";
    }
    final StringBuilder sb = new StringBuilder();
    sb.append('[').append(((Text) c).text);
    for (final Command s : c.getSubCommands()) {
      sb.append(' ').append(shape(s));
    }
    return sb.append(']').toString();
  }

  private static final Function<Command, String> OWN_ENCODER = c -> ((Text) c).text;
  private static final Function<String, Command> OWN_DECODER = s -> new Text(s);

  // --- the reference algorithms, copied from GameModule ---

  private static String referenceEncode(Command c) {
    if (c == null) {
      return null;
    }
    String s = OWN_ENCODER.apply(c);
    final Command[] sub = c.getSubCommands();
    if (sub.length > 0) {
      final SequenceEncoder se = new SequenceEncoder(s, DELIM);
      for (final Command command : sub) {
        final String s2 = referenceEncode(command);
        if (s2 != null) {
          se.append(s2);
        }
      }
      s = se.getValue();
    }
    return s;
  }

  private static Command referenceDecode(String command) {
    if (command == null) {
      return null;
    }
    Command c;
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(command, DELIM);
    final String first = st.nextToken();
    if (command.equals(first)) {
      c = OWN_DECODER.apply(first);
    }
    else {
      c = referenceDecode(first);
      while (st.hasMoreTokens()) {
        final Command next = referenceDecode(st.nextToken());
        c = c == null ? next : c.append(next);
      }
    }
    return c;
  }

  // --- the streaming ones under test ---

  private static String streamEncode(Command c) throws IOException {
    final StringWriter w = new StringWriter();
    CommandSerializer.write(c, OWN_ENCODER, DELIM, w);
    return w.toString();
  }

  private static Command streamDecode(String s) throws IOException {
    return CommandSerializer.read(new StringReader(s), CommandSerializerTest::referenceDecode, OWN_DECODER, DELIM);
  }

  private static void check(Command tree) throws IOException {
    final String reference = referenceEncode(tree);
    final String streamed = streamEncode(tree);
    assertEquals(reference == null ? "" : reference, streamed, "text written for " + shape(tree));

    if (reference != null) {
      final String expected = shape(referenceDecode(reference));
      assertEquals(expected, shape(streamDecode(reference)), "tree read from " + reference.replace(DELIM, '|'));
      assertEquals(expected, shape(streamDecode(streamed)));
      assertEquals(expected, shape(referenceDecode(streamed)));
    }
  }

  private static Text t(String text, Command... subs) {
    return new Text(text, subs);
  }

  @Test
  public void singleCommand() throws IOException {
    check(t("begin_save"));
    check(t(""));
    check(t("has" + DELIM + "delimiter"));
    check(t("\\starts with backslash"));
    check(t("'quoted'"));
    check(t("'"));
  }

  @Test
  public void nullRootWritesNothing() throws IOException {
    assertEquals("", streamEncode(null));
    assertEquals("", streamEncode(t(null)));
    assertNull(referenceEncode(t(null)));
  }

  @Test
  public void savedGameShape() throws IOException {
    // begin_save, a version check with a nested alert, the pieces under a
    // command with no text of its own, components, end_save
    final Text pieces = t(null);
    for (int i = 0; i < 50; ++i) {
      pieces.append(t("+/" + i + "/piece;;;img;Unit " + i + "\tmark;a/null;0;0;" + i));
    }
    final Text save = t("begin_save",
      t("COND;x<1", t("ALERT;too old")),
      pieces,
      t("EXT\tsomething\t1.0"),
      t("DECK\t..."),
      t("end_save"));
    check(save);
  }

  @Test
  public void subCommandsThatEncodeToNullAreSkipped() throws IOException {
    check(t("a", t(null), t("b"), t(null)));
    check(t("a", t(null)));
    check(t("a", t(null, t(null))));
    check(t(null, t(null, t("x"))));
  }

  @Test
  public void emptyTextsAreKeptAsEmptyTokens() throws IOException {
    check(t("a", t("")));
    check(t("", t("")));
    check(t("a", t(""), t("b")));
    check(t("", t("x"), t("")));
  }

  @Test
  public void delimitersInsideTextsAreEscapedByDepth() throws IOException {
    check(t("a" + DELIM, t("b" + DELIM + "c", t("d" + DELIM))));
    check(t(DELIM + "leading", t(String.valueOf(DELIM), t("" + DELIM + DELIM))));
    check(t("a", t("x" + DELIM + "y", t("p"), t("q" + DELIM))));
  }

  @Test
  public void quotingFollowsSequenceEncoder() throws IOException {
    // a sub-command whose whole text starts with a backslash
    check(t("a", t("\\x", t("y"))));
    check(t("a", t("\\x")));
    // whose whole text starts and ends with a quote
    check(t("a", t("'x", t("y'"))));
    check(t("a", t("'x'")));
    check(t("a", t("'", t("z'"))));
    // whose own text alone would be quoted but the whole is not
    check(t("a", t("'x'", t("y"))));
    check(t("'own'", t("b")));
    check(t("\\own", t("b")));
    // a quoted text at the root is written as it is
    check(t("'root'", t("b'")));
    // nested quoting
    check(t("a", t("'x", t("'y", t("z'")))));
  }

  @Test
  public void logShapeWithDelimitersInOwnText() throws IOException {
    // A LOG command's own text embeds the encoding of another tree, delimiters and all.
    final String inner = referenceEncode(t("M/1/2", t("D/3"), t("D/4")));
    check(t("begin_save", t("+/1/x/y"), t("end_save"), t("LOG\t" + inner), t("LOG\t" + inner, t("UNDO"))));
  }

  @Test
  public void randomTrees() throws IOException {
    final Random rnd = new Random(20260929);
    for (int i = 0; i < 3000; ++i) {
      check(randomTree(rnd, 0));
    }
  }

  private static final String ALPHABET = "ab" + DELIM + "\\'x";

  private static Command randomTree(Random rnd, int depth) {
    final int kind = rnd.nextInt(10);
    final String text;
    if (kind == 0) {
      text = null;
    }
    else if (kind == 1) {
      text = "";
    }
    else {
      final int len = 1 + rnd.nextInt(4);
      final StringBuilder sb = new StringBuilder();
      for (int i = 0; i < len; ++i) {
        sb.append(ALPHABET.charAt(rnd.nextInt(ALPHABET.length())));
      }
      text = sb.toString();
    }
    final List<Command> subs = new ArrayList<>();
    if (depth < 4) {
      final int n = rnd.nextInt(4);
      for (int i = 0; i < n; ++i) {
        subs.add(randomTree(rnd, depth + 1));
      }
    }
    return new Text(text, subs.toArray(new Command[0]));
  }
}
