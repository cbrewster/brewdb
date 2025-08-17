pub const InnerNodeHeader = struct {
    num_children: u8 = 0,

    // Partial prefix allows for optimistic path compression by merging multiple
    // single-child chains of nodes into a single node.
    //
    // The partial prefix has a max length of 8 to reduce size, once the prefix
    // exceeds 8, we switch to a pessimistic path compression approach where
    // we must validate the key during lookup by comparing it with the full key
    // in the leaf node.
    partial_prefix_raw: [8]u8 = [_]u8{0} ** 8,

    // The length of the compressed key path in this node. This may be greater than
    // the size of the partial prefix. In that case, the search algorithm must
    // validate the final key value stored in the leaf node.
    prefix_len: u64 = 0,
};
