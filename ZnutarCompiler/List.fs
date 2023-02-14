namespace Znutar

/// A list that contains at least one item.
type NonEmptyList<'t> =
    {
        Head : 't
        Tail : List<'t>
    }

    /// Number of items in this list.
    member this.Length =
        this.Tail.Length + 1

    /// All items in this list.
    member private this.Items =
        seq {
            yield this.Head
            yield! this.Tail
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
    let create head tail =
        {
            Head = head
            Tail = tail
        }

    /// Creates a non-empty list containing only the given item.
    let singleton head =
        create head []

    /// Maps the given function over the given non-empty
    /// list.
    let map mapping neList =
        create
            (mapping neList.Head)
            (List.map mapping neList.Tail)

    /// Converts the given non-empty list to a plain list.
    let toList neList =
        neList.Head :: neList.Tail

    /// Adds the given item to the front of the given non-empty
    /// list.
    let cons item neList =
        create item (toList neList)

    /// Reverses the given non-empty list.
    let rev neList =
        let init = create neList.Head []
        (init, neList.Tail)
            ||> List.fold (fun acc item ->
                create item (toList acc))

    /// Appends the second given list to the first.
    let append neList1 neList2 =
        create
            neList1.Head
            (neList1.Tail @ toList neList2)

/// A list that contains at least two items.
type MultiItemList<'t> =
    {
        Head1 : 't
        Head2 : 't
        Tail : List<'t>
    }

    /// Number of items in this list.
    member this.Length =
        this.Tail.Length + 2

    /// All items in this list.
    member private this.Items =
        seq {
            yield this.Head1
            yield this.Head2
            yield! this.Tail
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
    let create head1 head2 tail =
        {
            Head1 = head1
            Head2 = head2
            Tail = tail
        }

    /// Maps the given function over the given multi-item
    /// list.
    let map mapping milist =
        {
            Head1 = mapping milist.Head1
            Head2 = mapping milist.Head2
            Tail = List.map mapping milist.Tail
        }
