package VASSAL.build.module;

import VASSAL.build.GameModule;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.command.DeckRepositionCommand;
import VASSAL.counters.Deck;
import VASSAL.counters.GamePiece;
import VASSAL.tools.SequenceEncoder;
import java.awt.Point;

public final class DeckReposition implements CommandEncoder {

  private Deck deck;
  private Point newPosition;

  private static final char DELIMITER = '\t'; //$NON-NLS-1$
  public  static final String COMMAND_PREFIX = "DECKREPOS" + DELIMITER; //$NON-NLS-1$

  public DeckReposition(Deck deck, Point newPosition) {
    deck.setPosition(newPosition);
  }

  @Override
  public String encode(final Command c) {
    if (c instanceof DeckRepositionCommand) {
      final DeckRepositionCommand drc = (DeckRepositionCommand) c;
      final SequenceEncoder se = new SequenceEncoder(DELIMITER);
      se.append(deck.getId())
        .append(deck.getPosition().x)
        .append(deck.getPosition().y);
      return COMMAND_PREFIX + se.getValue();
    }
    return null;
  }

  /**
   * Deserializes string command info into a Deck Reposition Command.
   * @param s String for a Deck Reposition command string
   * @return Deck Reposition Command object
   */
  @Override
  public Command decode(final String s) {
    Deck deck = null;
    if (s.startsWith(COMMAND_PREFIX)) { // Make sure this command is for a Deck Reposition
      final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, DELIMITER);
      sd.nextToken(); // Skip over the Command Prefix
      final String decodedDeckId = sd.next();
      //Scan pieces to find the Deck
      for (final GamePiece piece : GameModule.getGameModule().getGameState().getAllPieces()) {
        if (piece instanceof Deck) {
          final String deckId = ((Deck) piece).getId();
          if (deckId.equals(decodedDeckId)) {
            deck =  (Deck) piece;
            break;
          }
        }
      }
      if (deck==null) {return null;};
      final int x = sd.nextInt(0);
      final int y = sd.nextInt(0);
      final Point newPosition = new Point(x, y);
      return new DeckRepositionCommand(deck,newPosition);
    }
    return null;
  }
}