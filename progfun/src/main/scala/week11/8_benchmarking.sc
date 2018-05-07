import org.scalameter
import org.scalameter._

val time = withWarmer(new scalameter.Warmer.Default) measure {
  (0 until 100000).toArray
}

val time2 = config(
  Key.exec.minWarmupRuns -> 20,
  Key.exec.maxWarmupRuns -> 60,
  Key.verbose -> true
) withWarmer(new Warmer.Default) measure {
  (0 until 1000000).toArray
}

withMeasurer(new Measurer.MemoryFootprint) measure { (0 until 1000000).toArray }