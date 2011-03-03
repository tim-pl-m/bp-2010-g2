
object parallel {
	def main(args: Array[String]){
            val pong = new Pong
            val ping = new Ping(1000000, pong)
            ping.start
            pong.start
          }
}

