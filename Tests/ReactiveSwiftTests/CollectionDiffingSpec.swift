import Nimble
import Quick
import ReactiveSwift
import Result
import Foundation
#if os(Linux)
import Glibc
#else
import Darwin.C
#endif

class CollectionDiffingSpec: QuickSpec {
	override func spec() {
		describe("diff()") {
			var snapshots: Signal<[Int], NoError>!
			var snapshotObserver: Signal<[Int], NoError>.Observer!
			var deltas: Signal<CollectionDelta<[Int]>, NoError>!

			beforeEach {
				let pipe = Signal<[Int], NoError>.pipe()
				(snapshots, snapshotObserver) = (pipe.output, pipe.input)
				deltas = snapshots.diff()
			}

			it("should produce a delta that can reproduce the current snapshot from the previous snapshot") {
				let oldNumbers = (0 ..< 32).shuffled()
				let newNumbers = (oldNumbers.dropLast(8) + (128 ..< 168)).shuffled()

				var delta: CollectionDelta<[Int]>?
				deltas.observeValues { delta = $0 }
				expect(delta).to(beNil())

				snapshotObserver.send(value: oldNumbers)
				snapshotObserver.send(value: newNumbers)
				expect(delta).toNot(beNil())

				if let delta = delta {
					var numbers = delta.previous
					expect(numbers) == oldNumbers

					delta.removals
						.union(IndexSet(delta.moves.lazy.map { $0.previous }))
						.reversed()
						.forEach { numbers.remove(at: $0) }

					delta.mutations.forEach { numbers[$0] = delta.current[$0] }

					delta.inserts
						.union(IndexSet(delta.moves.lazy.map { $0.current }))
						.forEach { numbers.insert(delta.current[$0], at: $0) }

					expect(numbers) == newNumbers
				}
			}
		}
	}
}

private extension Collection {
	func shuffled() -> [Iterator.Element] {
		var elements = Array(self)

		for i in elements.startIndex ..< elements.endIndex {
			let random = randomInteger() % elements.count
			guard random != i else { continue }
			#if swift(>=3.2)
				elements.swapAt(i, random)
			#else
				swap(&elements[i], &elements[random])
			#endif
		}

		return elements
	}
}

#if os(Linux)
	private func randomInteger() -> Int {
		srandom(UInt32(time(nil)))
		return Int(random() >> 1)
	}
#else
	private func randomInteger() -> Int {
		return Int(arc4random() >> 1)
	}
#endif
