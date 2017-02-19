import scala.collection.mutable.HashMap
import scala.collection.mutable.HashMap

object EntryPoint {
	val swarmSize = 20
	val maxIterations = 1000
	var swarm = new Array[Particle](swarmSize)

	def main(args: Array[String]): Unit = {
		// Create swarm
		for (i <- 0 to swarmSize - 1) {
			swarm(i) = new Particle
		}

		search()
	}

	def search() = {
		var currIteration = 0
		var foundSolution = false

		while(currIteration < maxIterations && !foundSolution) {
			for(particle <- swarm) {
				var objectiveFunctionResult = Particle.objectiveFunction(particle.getCurrentPosition())

				if(objectiveFunctionResult("result") < particle.getPersonalBest()("result"))
					particle.setPersonalBest(objectiveFunctionResult)

				if(particle.getPersonalBest()("result") < Particle.getNeighbourhoodBest()("result"))
					Particle.setNeighbourhoodBest(particle.getPersonalBest())
			}

			for(particle <- swarm) {
				particle.updateVelocity()
				particle.updatePosition()
			}

			currIteration += 1
		}

		printf("Best solution found was: %.6f at position %.6f\n", Particle.getNeighbourhoodBest()("result"), Particle.getNeighbourhoodBest()("x"))
	}

	class Particle(var c1: Double, var c2: Double, var w: Double) {
		val id = Particle.newId
		var currentVelocity = HashMap("x" -> 0.5)
		var currentPosition = HashMap("x" -> 0.0)
		var personalBest = Particle.objectiveFunction(currentPosition)
		
		init

		def this() = {
			this(0.1, 0.1, 0.0)
		}

		def init(): Unit = {
			this.setC1(c1)
			this.setC2(c2)
			this.setW(w)

			if(id == 0) {
				Particle.setNeighbourhoodBest(this.personalBest)
			}
		}

		def getId() : Int = id
		def getC1() : Double = c1
		def getC2() : Double = c2
		def getW()  : Double = w
		def getCurrentVelocity() : HashMap[String, Double] = currentVelocity
		def getCurrentPosition() : HashMap[String, Double] = currentPosition
		def getPersonalBest() : HashMap[String, Double] = personalBest

		def setC1(c1 : Double) : Unit = {
			this.c1 = c1
		}

		def setC2(c2 : Double) : Unit = {
			this.c2 = c2
		}

		def setW(w : Double) : Unit = {
			this.w = w
		}

		def setCurrentVelocity(currentVelocity : HashMap[String, Double]) : Unit = {
			this.currentVelocity = currentVelocity
		}

		def setCurrentPosition(currentPosition : HashMap[String, Double]) : Unit = {
			this.currentPosition = currentPosition
		}

		def setPersonalBest(personalBest : HashMap[String, Double]) : Unit = {
			this.personalBest = personalBest
		}

		def updateVelocity() : Unit = {
			this.currentVelocity = Particle.compunentWiseVectorAddition(this.currentVelocity, Particle.compunentWiseVectorAddition(cognitiveWeight(), socialWeight()))
		}

		def cognitiveWeight() : HashMap[String, Double] = {
			val r1 = Particle.getRandomVector(currentPosition)
			return Particle.compunentWiseVectorMultiplication(Particle.scalarTimesVector(c1, r1), Particle.compunentWiseVectorSubtraction(personalBest, currentPosition))
		}

		def socialWeight() : HashMap[String, Double] = {
			val r2 = Particle.getRandomVector(currentPosition)
			return Particle.compunentWiseVectorMultiplication(Particle.scalarTimesVector(c2, r2), Particle.compunentWiseVectorSubtraction(Particle.getNeighbourhoodBest(), currentPosition))
		}

		def updatePosition() : Unit = {
			this.currentPosition = Particle.compunentWiseVectorAddition(currentPosition, currentVelocity)
		}

		override def toString() : String = {
			return "Particle {" +
				"\tid: " + this.id + ",\n" +
				"\tc1: " + this.c1 + ",\n" +
				"\tc2: " + this.c2 + ",\n" +
				"\tw: " + this.w + "\n" +
			"}"
		}
	}

	object Particle {
		var random = scala.util.Random
		private var idCounter = 0
		private var neighbourhoodBest = new HashMap[String, Double]()

		private def newId() : Int = {
			idCounter += 1;
			return idCounter - 1;
		}

		def getNeighbourhoodBest() : HashMap[String, Double] = neighbourhoodBest

		def setNeighbourhoodBest(neighbourhoodBest : HashMap[String, Double]) : Unit = {
			this.neighbourhoodBest = neighbourhoodBest
		}

		def compareAndSetNeighbourhoodBest(neighbourhoodBest : HashMap[String, Double]) : Boolean = {
			if(this.neighbourhoodBest("result") > neighbourhoodBest("result")){
				this.neighbourhoodBest = neighbourhoodBest
				return true
			}
			else
				return false
		}

		def objectiveFunction(position : HashMap[String, Double]): HashMap[String, Double] = {
			return spherical(position)
		}

		def spherical(position : HashMap[String, Double]): HashMap[String, Double] = {
			return HashMap("x" -> position("x"),
				"result" -> 1)
		}

		def getRandomVector(vectorTemplate: HashMap[String, Double]) : HashMap[String, Double] = {
			var result = new HashMap[String, Double]()
			vectorTemplate.foreach((keyvalue) => result(keyvalue._1) = random.nextInt())
			return result
		}

		def scalarTimesVector(s: Double, v: HashMap[String, Double]) : HashMap[String, Double] = {
			var result = new HashMap[String, Double]()
			v.foreach((keyvalue) => result(keyvalue._1) = keyvalue._2 * s)
			return result
		}

		def compunentWiseVectorAddition(v1: HashMap[String, Double], v2: HashMap[String, Double]) : HashMap[String, Double] = {
			var result = new HashMap[String, Double]()
			v1.foreach((keyvalue) => {
				if(v2.contains(keyvalue._1))
					result(keyvalue._1) = keyvalue._2 + v2(keyvalue._1)	
			})
			return result
		}

		def compunentWiseVectorSubtraction(v1: HashMap[String, Double], v2: HashMap[String, Double]) : HashMap[String, Double] = {
			var result = new HashMap[String, Double]()
			v1.foreach((keyvalue) => {
				if(v2.contains(keyvalue._1))
					result(keyvalue._1) = keyvalue._2 - v2(keyvalue._1)	
			})
			return result
		}

		def compunentWiseVectorMultiplication(v1: HashMap[String, Double], v2: HashMap[String, Double]) : HashMap[String, Double] = {
			var result = new HashMap[String, Double]()
			v1.foreach((keyvalue) => {
				if(v2.contains(keyvalue._1))
					result(keyvalue._1) = keyvalue._2 * v2(keyvalue._1)	
			})
			return result
		}
	}
}