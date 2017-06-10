import Foundation
import Result

/// Represents an atomic batch of changes made to a collection.
///
/// A collection delta contains relative positions of elements within the collection. It
/// is safe to use these offsets directly as indices when `Elements` is statically known
/// to be container types like `Array` and `ContiguousArray`. However, the offsets should
/// not be used directly in a generic context. Refer to the documentations of these
/// offsets for how they should be consumed correctly.
public struct CollectionDelta<Elements: Collection>: CollectionDeltaProtocol {
	/// The collection before the changes were applied.
	public let previous: Elements

	/// The collection after the changes were applied.
	public let current: Elements

	/// The relative positions of inserted elements **after** the removals were applied.
	/// These are valid only with the `current` snapshot.
	///
	/// - important: To obtain the actual index, you must query the `index(_:offsetBy:)`
	///              method on `current`.
	public var inserts = IndexSet()

	/// The relative positions of removed elements **prior to** any changes being applied.
	/// These are valid only with the `previous` snapshot.
	///
	/// - important: To obtain the actual index, you must query the `index(_:offsetBy:)`
	///              method on `previous`.
	public var removals = IndexSet()

	/// The relative positions of mutations. These are valid with both the `previous` and
	/// the `current` snapshot.
	///
	/// Mutations imply the same relative position, but the actual indexes could be
	/// different after the changes were applied.
	///
	/// - important: To obtain the actual index, you must query the `index(_:offsetBy:)`
	///              method on either `previous` or `current`, depending on whether the
	///              old value or the new value is the interest.
	public var mutations = IndexSet()

	/// The relative movements. The `previous` offsets are valid with the `previous`
	/// snapshot, and the `current` offsets are valid with the `current` snapshot.
	///
	/// - important: To obtain the actual index, you must query the `index(_:offsetBy:)`
	///              method on either `previous` or `current` as appropriate.
	public var moves = [(previous: Int, current: Int)]()

	public init(previous: Elements, current: Elements) {
		(self.previous, self.current) = (previous, current)
	}
}

/// A protocol that can be used to constrain associated types as collection deltas.
public protocol CollectionDeltaProtocol {
	associatedtype Elements: Collection

	/// The collection before the changes were applied.
	var previous: Elements { get }

	/// The collection after the changes were applied.
	var current: Elements { get }

	/// The relative positions of inserted elements **after** the removals were applied.
	/// These are valid only with the `current` snapshot.
	///
	/// - important: To obtain the actual index, you must query the `index(_:offsetBy:)`
	///              method on `current`.
	var inserts: IndexSet { get }

	/// The relative positions of removed elements **prior to** any changes being applied.
	/// These are valid only with the `previous` snapshot.
	///
	/// - important: To obtain the actual index, you must query the `index(_:offsetBy:)`
	///              method on `previous`.
	var removals: IndexSet { get }

	/// The relative positions of mutations. These are valid with both the `previous` and
	/// the `current` snapshot.
	///
	/// Mutations imply the same relative position, but the actual indexes could be
	/// different after the changes were applied.
	///
	/// - important: To obtain the actual index, you must query the `index(_:offsetBy:)`
	///              method on either `previous` or `current`, depending on whether the
	///              old value or the new value is the interest.
	var mutations: IndexSet { get }

	/// The relative movements. The `previous` offsets are valid with the `previous`
	/// snapshot, and the `current` offsets are valid with the `current` snapshot.
	///
	/// - important: To obtain the actual index, you must query the `index(_:offsetBy:)`
	///              method on either `previous` or `current` as appropriate.
	var moves: [(previous: Int, current: Int)] { get }
}

extension Signal where Value: Collection & ExpressibleByArrayLiteral, Value.Iterator.Element: Hashable, Value.Indices.Iterator.Element == Value.Index {
	/// Compute the difference of `self` with regard to `old`.
	///
	/// `diff(with:)` works best with collections that contain unique values.
	///
	/// If the elements are repeated per the definition of `Element.==`, `diff(with:)`
	/// cannot guarantee a deterministic stable order, so these would all be uniformly
	/// treated as removals and inserts.
	///
	/// - precondition: `Self` must exhibit array semantics.
	///
	/// - complexity: O(n) time and space.
	public func diff() -> Signal<CollectionDelta<Value>, Error> {
		return Signal<CollectionDelta<Value>, Error> { observer in
			var _previous: Value?

			return self.observe { event in
				switch event {
				case let .value(elements):
					if let previous = _previous {
						observer.send(value: elements.diff(with: previous))
					} else {
						var diff = CollectionDelta<Value>(previous: [], current: elements)
						diff.inserts.insert(integersIn: 0 ..< Int(elements.count))
						observer.send(value: diff)
					}
					_previous = elements

				case .completed:
					observer.sendCompleted()

				case let .failed(error):
					observer.send(error: error)

				case .interrupted:
					observer.sendInterrupted()
				}
			}
		}
	}
}
extension Signal where Value: Collection & ExpressibleByArrayLiteral, Value.Iterator.Element: AnyObject, Value.Indices.Iterator.Element == Value.Index {
	/// Compute the difference of `self` with regard to `old`.
	///
	/// - complexity: O(n) time and space.
	public func diff() -> Signal<CollectionDelta<Value>, Error> {
		return Signal<CollectionDelta<Value>, Error> { observer in
			var _previous: Value?

			return self.observe { event in
				switch event {
				case let .value(elements):
					if let previous = _previous {
						observer.send(value: elements.diff(with: previous))
					} else {
						var diff = CollectionDelta<Value>(previous: [], current: elements)
						diff.inserts.insert(integersIn: 0 ..< Int(elements.count))
						observer.send(value: diff)
					}
					_previous = elements

				case .completed:
					observer.sendCompleted()

				case let .failed(error):
					observer.send(error: error)

				case .interrupted:
					observer.sendInterrupted()
				}
			}
		}
	}
}

extension SignalProducer where Value: Collection & ExpressibleByArrayLiteral, Value.Iterator.Element: Hashable, Value.Indices.Iterator.Element == Value.Index {
	/// Compute the difference of `self` with regard to `old`.
	///
	/// `diff(with:)` works best with collections that contain unique values.
	///
	/// If the elements are repeated per the definition of `Element.==`, `diff(with:)`
	/// cannot guarantee a deterministic stable order, so these would all be uniformly
	/// treated as removals and inserts.
	///
	/// - precondition: `Self` must exhibit array semantics.
	///
	/// - complexity: O(n) time and space.
	public func diff() -> SignalProducer<CollectionDelta<Value>, Error> {
		return lift { $0.diff() }
	}
}

extension SignalProducer where Value: Collection & ExpressibleByArrayLiteral, Value.Iterator.Element: AnyObject, Value.Indices.Iterator.Element == Value.Index {
	/// Compute the difference of `self` with regard to `old`.
	///
	/// - complexity: O(n) time and space.
	public func diff() -> SignalProducer<CollectionDelta<Value>, Error> {
		return lift { $0.diff() }
	}
}

extension PropertyProtocol where Value: Collection & ExpressibleByArrayLiteral, Value.Iterator.Element: Hashable, Value.Indices.Iterator.Element == Value.Index {
	/// Compute the difference of `self` with regard to `old`.
	///
	/// `diff(with:)` works best with collections that contain unique values.
	///
	/// If the elements are repeated per the definition of `Element.==`, `diff(with:)`
	/// cannot guarantee a deterministic stable order, so these would all be uniformly
	/// treated as removals and inserts.
	///
	/// - precondition: `Self` must exhibit array semantics.
	///
	/// - complexity: O(n) time and space.
	public func diff() -> Property<CollectionDelta<Value>> {
		return lift { $0.diff() }
	}
}

extension PropertyProtocol where Value: Collection & ExpressibleByArrayLiteral, Value.Iterator.Element: AnyObject, Value.Indices.Iterator.Element == Value.Index {
	/// Compute the difference of `self` with regard to `old`.
	///
	/// - complexity: O(n) time and space.
	public func diff() -> Property<CollectionDelta<Value>> {
		return lift { $0.diff() }
	}
}

// MARK: - Implementation details

private final class DiffEntry {
	var occurenceInOld = 0
	var occurenceInNew = 0
	var locationInOld: Int?
}

private enum DiffReference {
	case remote(Int)
	case table(DiffEntry)
}

private struct HashableDiffKey<Value: Hashable>: Hashable {
	let value: Value

	var hashValue: Int {
		return value.hashValue
	}

	init(_ value: Value) {
		self.value = value
	}

	static func ==(left: HashableDiffKey<Value>, right: HashableDiffKey<Value>) -> Bool {
		return left.value == right.value
	}
}

private struct ObjectPointerDiffKey: Hashable {
	let object: AnyObject

	var hashValue: Int {
		return ObjectIdentifier(object).hashValue
	}

	init(_ object: AnyObject) {
		self.object = object
	}

	static func ==(left: ObjectPointerDiffKey, right: ObjectPointerDiffKey) -> Bool {
		return left.object === right.object
	}
}

extension Collection where Iterator.Element: AnyObject, Indices.Iterator.Element == Index {
	fileprivate func diff(with old: Self) -> CollectionDelta<Self> {
		return diff(with: old, key: ObjectPointerDiffKey.init, equals: ===)
	}
}

extension Collection where Iterator.Element: Hashable, Indices.Iterator.Element == Index {
	fileprivate func diff(with old: Self) -> CollectionDelta<Self> {
		return diff(with: old, key: HashableDiffKey.init, equals: ==)
	}
}

extension Collection {
	fileprivate func diff<Key: Hashable>(
		with old: Self,
		key: (Iterator.Element) -> Key,
		equals: (Iterator.Element, Iterator.Element) -> Bool
	) -> CollectionDelta<Self> where Indices.Iterator.Element == Index {
		var table: [Key: DiffEntry] = Dictionary(minimumCapacity: Int(self.count))
		var oldReferences: [DiffReference] = []
		var newReferences: [DiffReference] = []

		oldReferences.reserveCapacity(Int(old.count))
		newReferences.reserveCapacity(Int(self.count))

		// Pass 1: Scan the new snapshot.
		for element in self {
			let key = key(element)
			let entry = table[key] ?? {
				let entry = DiffEntry()
				table[key] = entry
				return entry
			}()

			entry.occurenceInNew += 1
			newReferences.append(.table(entry))
		}

		// Pass 2: Scan the old snapshot.
		for (offset, index) in old.indices.enumerated() {
			let key = key(old[index])
			let entry = table[key] ?? {
				let entry = DiffEntry()
				table[key] = entry
				return entry
			}()

			entry.occurenceInOld += 1
			entry.locationInOld = offset
			oldReferences.append(.table(entry))
		}

		// Pass 3: Single-occurence lines
		for newPosition in newReferences.startIndex ..< newReferences.endIndex {
			switch newReferences[newPosition] {
			case let .table(entry):
				if entry.occurenceInNew == 1 && entry.occurenceInNew == entry.occurenceInOld {
					let oldPosition = entry.locationInOld!
					newReferences[newPosition] = .remote(oldPosition)
					oldReferences[oldPosition] = .remote(newPosition)
				}

			case .remote:
				break
			}
		}

		var diff = CollectionDelta<Self>(previous: old, current: self)

		// Final Pass: Compute diff.
		for position in oldReferences.indices {
			switch oldReferences[position] {
			case .table:
				// Deleted
				diff.removals.insert(position)

			case let .remote(newPosition):
				if newPosition == position {
					// Same line
					let oldIndex = old.index(old.startIndex, offsetBy: IndexDistance(position))
					let newIndex = self.index(self.startIndex, offsetBy: IndexDistance(newPosition))

					if !equals(old[oldIndex], self[newIndex]) {
						diff.mutations.insert(position)
					}
				} else {
					diff.moves.append((previous: position, current: newPosition))
				}
			}
		}

		for position in newReferences.indices {
			if case .table = newReferences[position] {
				diff.inserts.insert(position)
			}
		}

		return diff
	}
}

#if swift(>=3.2)
#else
	extension SignedInteger {
		fileprivate init<I: SignedInteger>(_ integer: I) {
			self.init(integer.toIntMax())
		}
	}
#endif

#if DEBUG
	extension Dictionary {
		fileprivate var prettyDebugDescription: String {
			return (["["] + self.map { "\t\($0.key): \($0.value)" } + ["]"]).joined(separator: "\n")
		}
	}

	extension Array {
		fileprivate var prettyDebugDescription: String {
			return (["["] + self.map { "\t\($0)" } + ["]"]).joined(separator: "\n")
		}
	}

	extension ObjectIdentifier {
		fileprivate var hexString: String {
			return String(hashValue, radix: 16, uppercase: true)
		}
	}

	extension DiffEntry: CustomStringConvertible {
		fileprivate var description: String {
			return "[addr \(ObjectIdentifier(self).hexString), #old \(occurenceInOld), #new \(occurenceInNew), oldLoc \(locationInOld.map(String.init(describing:)) ?? "nil")]"
		}
	}

	extension DiffReference: CustomStringConvertible {
		fileprivate var description: String {
			switch self {
			case let .remote(index):
				return "remote[\(index)]"

			case let .table(entry):
				return "entry[\(ObjectIdentifier(entry).hexString)]"
			}
		}
	}
#endif

