# Symbol vm-values

```
(vm-value :type :symbol :payload :symbol-keyword)
```

Let symbol literals be keywords, e.g. :point, :x, :y

Now we can do things like this:

```
(const foo :point) ; foo <- (vm-value :type :symbol :payload :point)
(struct-make p foo 2)
```

or just

```
(struct-make p :point 2)
```

# Structs are just arrays

We'll implement structs using one dimensional arrays. Field access is
the same as array element access.

# Option A - the one we'll use

## vm-value functions

The VM will keep track of struct definitions in a struct table. This
table is some data structure that keeps track of struct id mappings to
field id to field index mappings (a map of maps). The table is most
likely implemented with a map.

### (vm-value-define-struct (struct-table struct-id fields) ...)

Define a struct with the specified id and fields.

Needs check that no struct with this id is already defined and store
the struct definition.

e.g. `(vm-value-define-struct st :point '(:x :y))`

### (vm-make-struct (struct-table struct-id) ...)

Create a struct according to the struct definition referenced by
struct id. All fields are set to `none`.

e.g. `(vm-make-struct st :point)`

### (vm-value-struct-set struct-table struct field-id value)

Set the field referenced by `field-id` in `struct` to `value`.

- Assert types of the arguments
  - `struct` is `:struct`
  - `field-id` is `:symbol`
  - `value` is a `vm-value`
- Get field index for `field-id`
  - Signal an error if the field doens't exist
- Write `value` to the correct position in the underlying array

e.g `(vm-value-struct-set st p :x (vm-value-make-int 12))`

### (vm-value-struct-get struct-table struct field-id)

Return the value in the field referenced by `field-id` in `struct`.

- Assert types of the arguments
  - `struct` is `:struct`
  - `field-id` is `:symbol`
- Get field index for `field-id`
  - Signal an error if the field doens't exist
- Return the value at the correct position in the underlying array

e.g `(vm-value-struct-get st p :x)`

## VM instructions

### (define-struct struct-id field-name-0 field-name-1 ... field-name-n) ###

Define a struct with the specified id and fields.

- `struct-id` - a symbol identifying the struct type
- `field-name-n` - a symbol identifyign a field

e.g. `(define-struct :point :x :y)`

### (struct-make dst struct-id)

Create a struct with the specified struct-id.

e.g. `(struct-make p :point)`

### (struct-get dst struct field-name)

Get value specified by field name from struct.

- `dst` - destination register
- `struct` - a struct
- `field-name` - a symbol identifying a field

e.g. `(struct-get x p :x)`

### (struct-set struct field-name value)

Set the field identified by field-name in struct to value.

e.g. `(struct-set p :x 1000)`

### Example code

```
(define-struct :point :x :y)
(struct-make a-point :point)
(struct-set a-point :x 100)
(struct-set a-point :y 200)

;; assume foo contains a struct

(struct-get x foo :x)
(struct-get y foo :y)
```

# Option B - runner up

## (struct-make dst struct-id n)

Create a struct with the specified ID.

- `dst` - destination register
- `struct-id` - a symbol identifying the struct type
- `n` - number of fields in the struct

e.g. `(struct-make x point 2)`

## (struct-id dst struct)

Return the struct-id for the struct.

e.g. `(struct-id id foo)`

## (define-struct-slot struct-id field-name slot-idx)

Create a (struct-id, field-name) to slot index mapping and store it in the VM.

- `struct-id` - a symbol indentifying the struct type
- `field-name` - a symbol identifying the a field in the struct
- `slot-idx` - the index in the underlying array that will store the field value

e.g. `(define-struct-slot point x 0)`

## (struct-slot dst struct-id field-name)

Return the slot idx for field name in struct.

e.g. `(struct-slot idx point x)`

## Example code

```
(define-struct-slot :point :x 0)
(define-struct-slot :point :y 1)

(stuct-make a-point :point 2)

;; much later assume foo contains a point struct

(struct-id sid foo)
(struct-slot x-idx sid :x)
(vec-set foo x-idx 1000)
```
