/*
 *
 * Copyright (c) 2026 by vassalengine.org
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.build.module;

import javax.swing.JTextPane;
import javax.swing.text.Element;
import javax.swing.text.html.HTMLDocument;
import javax.swing.text.html.HTMLEditorKit;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class ChatterTest {

  /**
   * An {@code <html>} tag in a chat message opens two levels of nesting which are never closed, so a module
   * which wraps every report message in {@code <html>...</html>} deepens the conversation document without
   * bound and eventually overflows the stack when the pane is painted (#15300).
   */
  @Test
  public void testStripsDocumentTags() {
    assertEquals("hello", Chatter.stripDocumentTags("<html>hello</html>"));
    assertEquals("hello", Chatter.stripDocumentTags("<HTML>hello</HTML>"));
    assertEquals("hello", Chatter.stripDocumentTags("<Html>hello</Html>"));
    assertEquals("hello", Chatter.stripDocumentTags("<html >hello</html >"));
    assertEquals("hello", Chatter.stripDocumentTags("<html lang=\"en\">hello</html>"));
    assertEquals("hello", Chatter.stripDocumentTags("<html><body>hello</body></html>"));
    assertEquals("hello", Chatter.stripDocumentTags("<html><head></head><body>hello</body></html>"));
    assertEquals("a hello b", Chatter.stripDocumentTags("a <html>hello</html> b"));
  }

  /**
   * Only the document-structure tags go. Everything else a module may legitimately put in a report message
   * must survive untouched.
   */
  @Test
  public void testLeavesOtherMarkupAlone() {
    assertEquals("", Chatter.stripDocumentTags(""));
    assertEquals("plain text", Chatter.stripDocumentTags("plain text"));
    assertEquals("<b>bold</b>", Chatter.stripDocumentTags("<b>bold</b>"));
    assertEquals("<div class=\"msg\">x</div>", Chatter.stripDocumentTags("<div class=\"msg\">x</div>"));
    assertEquals("<img src=\"x.png\">", Chatter.stripDocumentTags("<img src=\"x.png\">"));
    assertEquals("<span style=\"color:#ff0000\">red</span>",
      Chatter.stripDocumentTags("<span style=\"color:#ff0000\">red</span>"));
    // Not a document tag, merely something which starts with the same letters
    assertEquals("<htmlish>x</htmlish>", Chatter.stripDocumentTags("<htmlish>x</htmlish>"));
    // Already escaped by the non-HTML path, so there is nothing to strip
    assertEquals("&lt;html&gt;x&lt;/html&gt;", Chatter.stripDocumentTags("&lt;html&gt;x&lt;/html&gt;"));
  }

  /**
   * The regression itself: feed messages to a conversation pane exactly as {@link Chatter} does, and check
   * that the document does not get deeper as messages arrive.
   */
  @Test
  public void testDocumentDoesNotDeepenPerMessage() throws Exception {
    final String message = "<html>* A unit moved to 1204</html>";

    assertTrue(depthAfter(message, 10) < depthAfter(message, 60),
      "expected an unstripped <html> wrapper to deepen the document, so that the check below means something");

    final String stripped = Chatter.stripDocumentTags(message);
    assertEquals(depthAfter(stripped, 10), depthAfter(stripped, 60),
      "a stripped message must leave the document depth unchanged however many arrive");
  }

  /**
   * Inserts {@code count} copies of {@code message} into a fresh conversation pane the way
   * {@code Chatter.doShow} does, and returns the depth of the resulting document.
   */
  private int depthAfter(String message, int count) throws Exception {
    final JTextPane pane = new JTextPane();
    pane.setContentType("text/html"); //NON-NLS
    final HTMLEditorKit kit = new HTMLEditorKit();
    pane.setEditorKit(kit);

    final HTMLDocument doc = (HTMLDocument) pane.getDocument();
    kit.insertHTML(doc, doc.getLength(), "<br>", 0, 0, null); //NON-NLS
    for (int i = 0; i < count; i++) {
      kit.insertHTML(doc, doc.getLength(), "\n<div class=\"msg\">" + message + "</div>", 0, 0, null); //NON-NLS
    }
    return depth(doc.getDefaultRootElement(), 1);
  }

  private int depth(Element e, int d) {
    int max = d;
    for (int i = 0; i < e.getElementCount(); i++) {
      max = Math.max(max, depth(e.getElement(i), d + 1));
    }
    return max;
  }
}
