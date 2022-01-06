all: build

build:
	sbt ';compile ;package'
	sbt ';Test/compile ;Test/package'

doc:
	sbt doc

genDistributedRouting:
	sbt 'Test/runMain tetriski.purlin.utils.genDistributedRouting'

genSourceRouting:
	sbt 'Test/runMain tetriski.purlin.utils.genSourceRouting'

genCircuitSwitched:
	sbt 'Test/runMain tetriski.purlin.utils.genCircuitSwitched'

testInjectionRate:
	sbt 'Test/runMain tetriski.purlin.utils.testInjectionRate'

testAlgorithms:
	sbt 'Test/runMain tetriski.purlin.utils.testAlgorithms'

testNetworkCompare:
	sbt 'Test/runMain tetriski.purlin.utils.testNetworkCompare'

clean:
	rm -f *.json *.fir *.v
	rm -rf test_run_dir/

cleanall: clean
	sbt clean
