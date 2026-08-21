/*
 *
 * Copyright (c) 2026 Christian Holm Christensen
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
package VASSAL.build;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.tools.SequenceEncoder;
import VASSAL.build.module.BasicLogger;
import VASSAL.build.module.GameComponent;
import VASSAL.tools.ErrorDialog;

public class Random extends java.util.Random
  implements CommandEncoder, GameComponent {
  protected static final String COMMAND_PREFIX = "RNG\t";
  
  protected boolean seedSeen = false;
  protected long storedSeed = 0;

  static final long serialVersionUID = 0L;

  /**
   * Get the next random value.  This registers the random value and
   * puts it to the current log file or clients listening.
   */
  @Override
  public int nextInt() {
    final int ret = super.nextInt();
    logRandom("I", Integer.valueOf(ret));
    return ret;
  }
  /**
   * Get the next random value.  This registers the random value and
   * puts it to the current log file or clients listening.
   */
  @Override
  public int nextInt(int bound) {
    final int ret = super.nextInt(bound);
    logRandom("M", Integer.valueOf(ret), Integer.valueOf(bound));
    return ret;
  }
  /**
   * Get the next random value.  This registers the random value and
   * puts it to the current log file or clients listening.
   */
  @Override
  public long nextLong() {
    final long ret = super.nextLong();
    logRandom("L", Long.valueOf(ret));
    return ret;
  }
  /**
   * Get the next random value.  This registers the random value and
   * puts it to the current log file or clients listening.
   */
  @Override
  public double nextDouble() {
    final double ret = super.nextDouble();
    logRandom("D", Double.valueOf(ret));
    return ret;
  }
  /**
   * Get the next random value.  This registers the random value and
   * puts it to the current log file or clients listening.
   */
  @Override
  public float nextFloat() {
    final float ret = super.nextFloat();
    logRandom("F", Float.valueOf(ret));
    return ret;
  }
  /**
   * Get the next random value.  This registers the random value and
   * puts it to the current log file or clients listening.
   */
  @Override
  public boolean nextBoolean() {
    final boolean ret = super.nextBoolean();
    logRandom("B", Boolean.valueOf(ret));
    return ret;
  }
  /**
   * Get the next random value.  This registers the random value and
   * puts it to the current log file or clients listening.
   */
  @Override
  public double nextGaussian() {
    final double ret = super.nextGaussian();
    logRandom("G", Double.valueOf(ret));
    return ret;
  }
  /**
   * Set the seed on this random number generator.
   *
   * This also registeres that the seed was set.  That means, as a
   * client, we will now check if the passed random values (via
   * commands) actually match the next random variable in the
   * sequence.  It also means, that if we're reading older log files,
   * where the seed isn't set, that we will not do those checks.
   */ 
  @Override
  public void setSeed(long seed) {
    super.setSeed(seed);
    storedSeed = seed;
  }
  public void restoreSeed(long seed) {
    setSeed(seed);
    seedSeen = true;
    // System.out.println("Restoring seed " + seed);
  }
  public long getSeed() {
    return storedSeed;
  }
    
  /**
   * This writes a command to the log.  The command is prefixed by our
   * prefix, and then followed by the random value.
   *
   * @param type  The kind of random variable
   * @param value  The value of the random value
   * @param bound  Possible bound
   */
  protected Command logRandom(String type, Object value, Object bound) {
    final RandomCommand c = new RandomCommand(type, value, bound);
    GameModule.getGameModule().sendAndLog(c);
    return c;
  }
  protected Command logRandom(String type, Object value) {
    return logRandom(type, value, null);
  }
  
  /*
   * Checks that the seen value is the next one in the random number
   * sequence. If not, an exception is thrown.
   */
  /**
   * Check that the passed value is the next value of the generator's sequence.
   * If not, then an exception is thrown.
   *
   * @param value Value to check
   */
  protected void checkInt(int value) {
    if (!seedSeen) {
      return;
    }
    
    final int here = super.nextInt();
    // System.out.println("Read=" + value + " here=" + here +
    //                    " seed=" + storedSeed);
    if (here != value) {
      ErrorDialog.showDetails("Read random integer value " + value +
                              " does not match next the expected value " +
                              here,
                              "Random.InconsistentNumber",
                              "integer", value, here);
    }
  }
  /**
   * Check that the passed value is the next value of the generator's
   * sequence.  If not, then an exception is thrown.
   *
   * @param value Value to check
   */
  protected void checkInt(int value, int bound) {
    if (!seedSeen) {
      return;
    }
    
    final int here = super.nextInt(bound);
    // System.out.println("Read=" + value + " here=" + here +
    //                    " seed=" + storedSeed);
    if (here != value) {
      ErrorDialog.showDetails("Read random bound integer value " + value +
                              " does not match the expected value " + here,
                              "Random.InconsistentNumber",
                              "bound integer", value, here);
    }
  }
  /**
   * Check that the passed value is the next value of the generator's
   * sequence.  If not, then an exception is thrown.
   *
   * @param value Value to check
   */
  protected void checkLong(long value) {
    if (!seedSeen) {
      return;
    }
    
    final long here = super.nextLong();
    // System.out.println("Read=" + value + " here=" + here +
    //                    " seed=" + storedSeed);
    if (here != value) {
      ErrorDialog.showDetails("Read random  long " + value +
                              " does not match expected value " + here,
                              "Random.InconsistentNumber",
                              "long", value, here);
    }
  }
  /**
   * Check that the passed value is the next value of the generator's
   * sequence.  If not, then an exception is thrown.
   *
   * @param value Value to check
   */
  protected void checkDouble(double value) {
    if (!seedSeen) {
      return;
    }
    
    final double here = super.nextDouble();
    // System.out.println("Read=" + value + " here=" + here +
    //                    " seed=" + storedSeed);
    if (here != value) {
      ErrorDialog.showDetails("Read random  double " + value +
                              " does not match expected value " + here,
                              "Random.InconsistentNumber",
                              "double", value, here);
    }
  }
  /**
   * Check that the passed value is the next value of the generator's
   * sequence.  If not, then an exception is thrown.
   *
   * @param value Value to check
   */
  protected void checkFloat(float value) {
    if (!seedSeen) {
      return;
    }
    
    final float here = super.nextFloat();
    // System.out.println("Read=" + value + " here=" + here +
    //                    " seed=" + storedSeed);
    if (here != value) {
      ErrorDialog.showDetails("Read random  float " + value +
                              " does not match expected value " + here,
                              "Random.InconsistentNumber",
                              "float", value, here);
    }
  }
  /**
   * Check that the passed value is the next value of the generator's sequence.
   * If not, then an exception is thrown.
   *
   * @param value Value to check
   */
  protected void checkBoolean(boolean value) {
    if (!seedSeen) {
      return;
    }
    
    final boolean here = super.nextBoolean();
    // System.out.println("Read=" + value + " here=" + here +
    //                    " seed=" + storedSeed);
    if (here != value) {
      ErrorDialog.showDetails("Read random  boolean " + value +
                              " does not match expected value " + here,
                              "Random.InconsistentNumber",
                              "boolean", value, here);
    }
  }
  /**
   * Check that the passed value is the next value of the generator's
   * sequence.  If not, then an exception is thrown.
   *
   * @param value Value to check
   */
  protected void checkGaussian(double value) {
    if (!seedSeen) {
      return;
    }
    
    final double here = super.nextGaussian();
    // System.out.println("Read=" + value + " here=" + here +
    //                    " seed=" + storedSeed);
    if (here != value) {
      ErrorDialog.showDetails("Read random  Gaussian " + value +
                              " does not match next random value " + here,
                              "Random.InconsistentNumber",
                              "Gaussian", value, here);
    }
  }

  /**
   * Component interface
   */
  @Override
  public void setup(boolean starting) {}

  /**
   * Command to add to save or log file to restore the random number
   * seed.  This command will be read and executed when a log file is
   * played-back.
   */
  @Override
  public Command getRestoreCommand() {
    // If we are not _actually_ recording a log, do nothing.
    final GameModule  module = GameModule.getGameModule();
    final BasicLogger logger = module != null ? module.getBasicLogger() : null;
    if (!(logger != null && logger.isLogging() /* && !logger.isReplaying() */)) {
      return null;
    }
    
    // Generate a random number with the previous seed - possibly one
    // read from a log file or from other client - so that we can
    // check that this log is indeed a continuation of a previous log.
    final long          val = (logger.isReplaying() ? storedSeed :
                               super.nextLong());
    final RandomCommand ret = new RandomCommand("L", Long.valueOf(val));
    ret.append(new RandomCommand("Z", Long.valueOf(val)));

    // We will use the above random number as the new seed.  In that
    // way, we have a predicatable seed (and hence sequence of random
    // numbers) which depends on that previous seed.  Thus, if the
    // user loaded a save or log, and then starts a new log, then the
    // seed will depend on that previous log.  If the user loaded the
    // save or log, and then filled around with the numbers, then the
    // new seed won't match, and we'll detect that.
    //
    // If no previous seed was seen, we make a new one. 
    // System.out.println("Set new seed " + val);
    setSeed(val);
    
    return ret;
  }
  /*
   * Command interface
   */
  /**
   * Decode a command.  We do this when, either
   * - we are reading from a log file (PBEM)
   * - or we are getting commands over network 
   *
   */
  @Override
  public Command decode(String command) {
    final SequenceEncoder.Decoder sd =
      new SequenceEncoder.Decoder(command, ';');
    final String prefix = sd.nextToken("");
    if (!prefix.equals(COMMAND_PREFIX)) {
      return null; // Not for us 
    }
    final String        type = sd.nextToken("");
    final RandomCommand cmd  = new RandomCommand(type);
    
    if (type.equals("Z")) {
      // Got a seed value 
      final long value = sd.nextLong(0);
      cmd.setValue(Long.valueOf(value));
      return cmd;
    }

    // We've not seen a seed, so we do nothing when we see a random
    // number log.  This is the case when a user is simply playing a
    // turn or similar, or if we're reading older log files without
    // this random number generator wrapper.
    if (type.equals("I")) {
      cmd.setValue(Integer.valueOf(sd.nextInt(0)));
    }
    else if (type.equals("M")) {
      cmd.setValue(Integer.valueOf(sd.nextInt(0)));
      cmd.setBound(Integer.valueOf(sd.nextInt(0)));
    }
    else if (type.equals("L")) {
      cmd.setValue(Long.valueOf(sd.nextLong(0)));
    }
    else if (type.equals("D")) {
      cmd.setValue(Double.valueOf(sd.nextDouble(0.)));
    }
    else if (type.equals("F")) {
      cmd.setValue(Float.valueOf((float)sd.nextDouble(0.)));
    }
    else if (type.equals("B")) {
      cmd.setValue(Boolean.valueOf(sd.nextBoolean(false)));
    }
    else if (type.equals("G")) {
      cmd.setValue(Double.valueOf(sd.nextDouble(0.)));
    }
    return cmd;
  }
  @Override
  public String encode(Command command) {
    if (!(command instanceof RandomCommand)) {
      return null;
    }

    final RandomCommand rc    = (RandomCommand)command;
    final String        type  = rc.getType();
    final Object        value = rc.getValue();
    final Object        bound = rc.getBound();

    final SequenceEncoder se = new SequenceEncoder(COMMAND_PREFIX, ';');
    se.append(type)
      .append(value.toString())
      .append(bound != null ? bound.toString() : "");
    
    return se.getValue() + "\\";
  }

  /**
   * Command to keep track of random numbers generated
   */
  public static class RandomCommand extends Command {
    protected String type;
    protected Object value;
    protected Object bound = null;

    public RandomCommand(String t) {
      this(t, null, 0);
    }
    public RandomCommand(String t, Object v) {
      this(t, v, 0);
    }
    public RandomCommand(String t, Object v, Object b) {
      type  = t;
      value = v;
      bound = b;
    }

    public void setValue(Object v) {
      value = v;
    }
    public void setBound(Object b) {
      bound = b;
    }
    public String getType() {
      return type;
    }
    public Object getValue() {
      return value;
    }
    public Object getBound() {
      return bound;
    }

    /**
     * Get our random number generator.  Note, it is cast to our type,
     * as we will always use an object of that type.  This allows us
     * to delegate checks to that type - particularly as we need to
     * check if we've seen a seed has been set.
     */
    protected Random getRNG() {
      return (Random)GameModule.getGameModule().getRNG();
    }
      

    /**
     * This checks that the random number we got is indeed the next
     * value in the sequence of random numbers.  This throws an
     * exception if it is not.
     */
    @Override   
    public void executeCommand() {
      if (type.equals("Z")) {
        getRNG().restoreSeed(((Long)value).longValue());
      }
      else if (type.equals("I")) {
        getRNG().checkInt(((Integer)value).intValue());
      }
      else if (type.equals("M")) {
        getRNG().checkInt(((Integer)value).intValue(),
                          ((Integer)bound).intValue());
      }
      else if (type.equals("L")) {
        getRNG().checkLong(((Long)value).longValue());
      }
      else if (type.equals("D")) {
        getRNG().checkDouble(((Double)value).doubleValue());
      }
      else if (type.equals("F")) {
        getRNG().checkFloat(((Float)value).floatValue());
      }
      else if (type.equals("B")) {
        getRNG().checkBoolean(((Boolean)value).booleanValue());
      }
      else if (type.equals("G")) {
        getRNG().checkGaussian(((Double)value).doubleValue());
      }
    }
    /**
     * This returns null, because we cannot undo random numbers
     */
    @Override
    protected Command myUndoCommand() {
      return null;
    }
  }
}

