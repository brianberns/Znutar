namespace Znutar

/// A list that contains at least one item.
type NonEmptyList<'t> =
    {
        Item : 't
        Rest : List<'t>
    }

    /// Number of items in this list.
    member this.Length =
        this.Rest.Length + 1

    /// All items in this list.
    member this.Items =
        seq {
            yield this.Item
            yield! this.Rest
        }

    interface System.Collections.Generic.IEnumerable<'t> with

        /// Supports iteration.
        member this.GetEnumerator() = 
            this.Items.GetEnumerator()
                :> System.Collections.IEnumerator

        /// Supports iteration.
        member this.GetEnumerator() =
            this.Items.GetEnumerator()

module NonEmptyList =

    /// Creates a non-empty list.
    let create item rest =
        {
            Item = item
            Rest = rest
        }

    /// Maps the given function over the given non-empty
    /// list.
    let map mapping neList =
        {
            Item = mapping neList.Item
            Rest = List.map mapping neList.Rest
        }

/// A list that contains at least two items.
type MultiItemList<'t> =
    {
        Item1 : 't
        Item2 : 't
        Rest : List<'t>
    }

    /// Number of items in this list.
    member this.Length =
        this.Rest.Length + 2

    /// All items in this list.
    member this.Items =
        seq {
            yield this.Item1
            yield this.Item2
            yield! this.Rest
        }

    interface System.Collections.Generic.IEnumerable<'t> with

        /// Supports iteration.
        member this.GetEnumerator() = 
            this.Items.GetEnumerator()
                :> System.Collections.IEnumerator

        /// Supports iteration.
        member this.GetEnumerator() =
            this.Items.GetEnumerator()

module MultiItemList =

    /// Creates a multi-item list.
    let create item1 item2 rest =
        {
            Item1 = item1
            Item2 = item2
            Rest = rest
        }

    /// Maps the given function over the given multi-item
    /// list.
    let map mapping milist =
        {
            Item1 = mapping milist.Item1
            Item2 = mapping milist.Item2
            Rest = List.map mapping milist.Rest
        }
