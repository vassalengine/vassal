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

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.function.Function;

/**
 * Streams a tree of {@link Command}s to and from the text form that
 * {@link VASSAL.build.GameModule#encode(Command)} produces and
 * {@link VASSAL.build.GameModule#decode(String)} reads, without ever
 * holding the whole text in memory.
 *
 * <p>The text form of a command tree is a {@link VASSAL.tools.SequenceEncoder}
 * sequence: the command's own text (as one of the module's
 * {@link CommandEncoder}s produces it) is the first token, and the text form
 * of each sub-command is a further token. A sub-command's text is itself such
 * a sequence, escaped as one token, so a delimiter belonging to a command
 * nested {@code d} levels deep carries {@code d} backslashes. A saved game is
 * such a tree, and its pieces sit two levels down, so on a large game the
 * String form is hundreds of megabytes while any one command is a few
 * kilobytes.</p>
 *
 * <p>{@link #write} produces exactly the bytes {@code encode} would, applying
 * each nesting level's escaping and quoting as it goes rather than
 * re-escaping the text of the levels below. {@link #read} consumes exactly
 * what {@code decode} would, splitting at each level's delimiters as the
 * characters stream past and materialising only one command's own text at a
 * time. Either side may therefore be the String-based one.</p>
 */
public final class CommandSerializer {
  private CommandSerializer() {
  }

  /**
   * Writes the text form of a command tree.
   *
   * @param c the root of the tree; nothing is written for {@code null}, or for
   *          a command with no text and no sub-commands (whose String form is
   *          {@code null})
   * @param ownEncoder gives a command's own text, or {@code null} if no
   *                   encoder recognises it
   * @param delim the delimiter between a command's text and its sub-commands
   * @param out the destination
   */
  public static void write(Command c, Function<Command, String> ownEncoder, char delim, Writer out) throws IOException {
    if (c != null) {
      new TreeWriter(ownEncoder, delim, out).writeRoot(c);
    }
  }

  /**
   * Reads the text form of a command tree.
   *
   * @param in the text
   * @param stringDecoder decodes a complete text form held as a String (the
   *                      module's own {@code decode(String)}), used for a
   *                      command's own text
   * @param ownDecoder decodes a single command's own text through the
   *                   module's {@link CommandEncoder}s
   * @param delim the delimiter between a command's text and its sub-commands
   * @return the tree, or {@code null} if the text decodes to nothing
   */
  public static Command read(Reader in, Function<String, Command> stringDecoder, Function<String, Command> ownDecoder, char delim) throws IOException {
    return new TreeReader(stringDecoder, ownDecoder, delim).decode(new ReaderSource(in));
  }

  private static boolean quoteNeeded(int first, int last) {
    // SequenceEncoder.append() wraps a token in single quotes if it starts
    // with a backslash, or both starts and ends with a single quote.
    return first == '\\' || (first == '\'' && last == '\'');
  }

  /**
   * The writer. A command's text form nested {@code depth} levels deep has
   * each delimiter escaped {@code depth} times. A command with sub-commands
   * is written as its own text (a token escaped once more, since it is a
   * token of the command's own sequence) followed by each sub-command whose
   * text form is not null, each preceded by a delimiter. The quoting of a
   * sub-command's whole text is decided by its first and last characters,
   * which are found by looking down its rightmost line of descent without
   * building anything.
   */
  private static final class TreeWriter {
    private final Function<Command, String> ownEncoder;
    private final char delim;
    private final Writer out;

    /**
     * Own texts computed while looking ahead, kept only until the command is
     * written, so nothing is encoded twice.
     */
    private final Map<Command, Optional<String>> lookedAhead = new IdentityHashMap<>();

    TreeWriter(Function<Command, String> ownEncoder, char delim, Writer out) {
      this.ownEncoder = ownEncoder;
      this.delim = delim;
      this.out = out;
    }

    void writeRoot(Command c) throws IOException {
      if (!encodesToNull(c)) {
        writeCommand(c, 0);
      }
    }

    private Optional<String> peekOwn(Command c) {
      return lookedAhead.computeIfAbsent(c, k -> Optional.ofNullable(ownEncoder.apply(k)));
    }

    private String takeOwn(Command c) {
      final Optional<String> o = lookedAhead.remove(c);
      return o != null ? o.orElse(null) : ownEncoder.apply(c);
    }

    /** True if encode(c) is null: no own text and no sub-commands. */
    private boolean encodesToNull(Command c) {
      return c.getSubCommands().length == 0 && peekOwn(c).isEmpty();
    }

    /** Writes encode(c), for a command whose encoding is not null, as it appears nested {@code depth} levels deep. */
    private void writeCommand(Command c, int depth) throws IOException {
      final String own = takeOwn(c);
      final Command[] subs = c.getSubCommands();
      if (subs.length == 0) {
        // A leaf's text form is its own text as it is, escaped only by its ancestors.
        writeEscaped(own, depth);
        return;
      }

      // Own text as the first token of this command's sequence.
      writeToken(own, depth + 1);

      for (final Command sub : subs) {
        if (encodesToNull(sub)) {
          continue;
        }
        writeBackslashes(depth);
        out.write(delim);
        final boolean quoted = quoteNeeded(firstChar(sub), lastChar(sub));
        if (quoted) {
          out.write('\'');
        }
        writeCommand(sub, depth + 1);
        if (quoted) {
          out.write('\'');
        }
      }
    }

    /** SequenceEncoder.append(s), as it appears nested {@code depth} levels deep. */
    private void writeToken(String s, int depth) throws IOException {
      if (s == null || s.isEmpty()) {
        return;
      }
      final boolean quoted = quoteNeeded(s.charAt(0), s.charAt(s.length() - 1));
      if (quoted) {
        out.write('\'');
      }
      writeEscaped(s, depth);
      if (quoted) {
        out.write('\'');
      }
    }

    /** Writes s with {@code n} backslashes before each delimiter in it. */
    private void writeEscaped(String s, int n) throws IOException {
      if (n == 0) {
        out.write(s);
        return;
      }
      int begin = 0;
      for (int i = s.indexOf(delim); i >= 0; i = s.indexOf(delim, i + 1)) {
        out.write(s, begin, i - begin);
        writeBackslashes(n);
        begin = i;
      }
      out.write(s, begin, s.length() - begin);
    }

    private void writeBackslashes(int n) throws IOException {
      for (int i = 0; i < n; ++i) {
        out.write('\\');
      }
    }

    /** The first character of encode(c), or -1 if that is empty; encode(c) must not be null. */
    private int firstChar(Command c) {
      final String own = peekOwn(c).orElse(null);
      final Command[] subs = c.getSubCommands();
      if (subs.length == 0) {
        return own.isEmpty() ? -1 : own.charAt(0);
      }
      if (own != null && !own.isEmpty()) {
        // The own token: quoted, or escaped, or as it is.
        final int first = own.charAt(0);
        if (quoteNeeded(first, own.charAt(own.length() - 1))) {
          return '\'';
        }
        return first == delim ? '\\' : first;
      }
      // An empty own token: the text starts with the first sub-command's delimiter, if any.
      for (final Command sub : subs) {
        if (!encodesToNull(sub)) {
          return delim;
        }
      }
      return -1;
    }

    /** The last character of encode(c), or -1 if that is empty; encode(c) must not be null. */
    private int lastChar(Command c) {
      final String own = peekOwn(c).orElse(null);
      final Command[] subs = c.getSubCommands();
      if (subs.length == 0) {
        return own.isEmpty() ? -1 : own.charAt(own.length() - 1);
      }
      for (int i = subs.length - 1; i >= 0; --i) {
        final Command sub = subs[i];
        if (encodesToNull(sub)) {
          continue;
        }
        final int last = lastChar(sub);
        if (quoteNeeded(firstChar(sub), last)) {
          return '\'';
        }
        // An empty last sub-command leaves the text ending with its delimiter.
        return last < 0 ? delim : last;
      }
      // No sub-command is written: the text is the own token alone.
      if (own == null || own.isEmpty()) {
        return -1;
      }
      final int last = own.charAt(own.length() - 1);
      return quoteNeeded(own.charAt(0), last) ? '\'' : last;
    }
  }

  /** A source of characters, ending with -1. */
  private interface CharSource {
    int read() throws IOException;
  }

  /** Reads a Reader in blocks, so that taking one character at a time costs no call into the Reader. */
  private static final class ReaderSource implements CharSource {
    private final Reader in;
    private final char[] buf = new char[1 << 14];
    private int pos;
    private int end;

    ReaderSource(Reader in) {
      this.in = in;
    }

    @Override
    public int read() throws IOException {
      if (pos == end) {
        end = in.read(buf, 0, buf.length);
        pos = 0;
        if (end <= 0) {
          end = 0;
          return -1;
        }
      }
      return buf[pos++];
    }
  }

  /**
   * Splits one level of a sequence into its tokens, as
   * {@link VASSAL.tools.SequenceEncoder.Decoder} does, but over a stream of
   * characters: a delimiter preceded by a backslash is part of the token
   * with that backslash dropped, any other delimiter ends the token and
   * another token follows (so text ending with a delimiter ends with an
   * empty token), and the end of the input ends the last token.
   */
  private static final class TokenReader {
    private final CharSource in;
    private final char delim;

    private boolean finished;
    private int pushback = NONE;
    private Token current;

    private static final int NONE = -2;

    TokenReader(CharSource in, char delim) {
      this.in = in;
      this.delim = delim;
    }

    boolean hasMoreTokens() {
      return !finished;
    }

    Token nextToken() throws IOException {
      if (finished) {
        throw new NoSuchElementException();
      }
      if (current != null) {
        current.drain();
      }
      current = new Token();
      return current;
    }

    private int readIn() throws IOException {
      if (pushback != NONE) {
        final int c = pushback;
        pushback = NONE;
        return c;
      }
      return in.read();
    }

    /** The characters of one token; -1 at its end. */
    final class Token implements CharSource {
      private boolean ended;
      private boolean escapeDropped;
      private int unread = NONE;

      /** @return true if an escaping backslash was dropped while reading this token */
      boolean escapeDropped() {
        return escapeDropped;
      }

      /** Pushes back the character just read from this token. */
      void unread(int c) {
        unread = c;
      }

      @Override
      public int read() throws IOException {
        if (unread != NONE) {
          final int c = unread;
          unread = NONE;
          return c;
        }
        if (ended) {
          return -1;
        }
        final int c = readIn();
        if (c < 0) {
          // The input ends with this token.
          ended = true;
          finished = true;
          return -1;
        }
        if (c == '\\') {
          final int next = readIn();
          if (next == delim) {
            escapeDropped = true;
            return delim;
          }
          // Not an escape: give the next character back. An end of input
          // is simply seen again on the next read.
          if (next >= 0) {
            pushback = next;
          }
          return '\\';
        }
        if (c == delim) {
          // A real delimiter: this token ends, another follows.
          ended = true;
          return -1;
        }
        return c;
      }

      void readAll(StringBuilder sb) throws IOException {
        for (int c = read(); c >= 0; c = read()) {
          sb.append((char) c);
        }
      }

      void drain() throws IOException {
        int c;
        do {
          c = read();
        } while (c >= 0);
      }
    }
  }

  /**
   * The reader, mirroring {@code GameModule.decode(String)}: the first token
   * is the command's own text, decoded on its own; if it was the whole text,
   * untouched by unescaping or unquoting, that is the command. Otherwise each
   * further token is a sub-command's text form, decoded the same way from
   * its own characters and appended.
   */
  private static final class TreeReader {
    private final Function<String, Command> stringDecoder;
    private final Function<String, Command> ownDecoder;
    private final char delim;

    TreeReader(Function<String, Command> stringDecoder, Function<String, Command> ownDecoder, char delim) {
      this.stringDecoder = stringDecoder;
      this.ownDecoder = ownDecoder;
      this.delim = delim;
    }

    Command decode(CharSource src) throws IOException {
      final TokenReader tr = new TokenReader(src, delim);
      final TokenReader.Token firstToken = tr.nextToken();
      final StringBuilder sb = new StringBuilder();
      firstToken.readAll(sb);

      boolean transformed = firstToken.escapeDropped();
      String first = sb.toString();
      if (isQuoted(first)) {
        first = first.substring(1, first.length() - 1);
        transformed = true;
      }

      if (!tr.hasMoreTokens() && !transformed) {
        // The text was a single command.
        return ownDecoder.apply(first);
      }

      Command c = stringDecoder.apply(first);
      while (tr.hasMoreTokens()) {
        final Command next = decodeToken(tr.nextToken());
        c = c == null ? next : c.append(next);
      }
      return c;
    }

    private Command decodeToken(TokenReader.Token tok) throws IOException {
      final int first = tok.read();
      if (first == '\'') {
        // Possibly a quoted token, which is only known once its end is seen:
        // take it whole, as the String decoder would.
        final StringBuilder sb = new StringBuilder();
        sb.append('\'');
        tok.readAll(sb);
        String s = sb.toString();
        if (isQuoted(s)) {
          s = s.substring(1, s.length() - 1);
        }
        return stringDecoder.apply(s);
      }
      if (first >= 0) {
        tok.unread(first);
      }
      return decode(tok);
    }

    private static boolean isQuoted(String s) {
      final int len = s.length();
      return len > 1 && s.charAt(0) == '\'' && s.charAt(len - 1) == '\'';
    }
  }
}
