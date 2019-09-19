## GET /list

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[]
```

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[{"id":42,"name":"Listen-Name","nrActive":11}]
```

- Example (`application/json;charset=utf-8`):

```javascript
[{"id":42,"name":"Listen-Name","nrActive":11},{"id":42,"name":"Listen-Name","nrActive":11}]
```

## POST /list

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
"Text"
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"id":42,"name":"Listen-Name","nrActive":11}
```

## PUT /list

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"id":42,"name":"Listen-Name","nrActive":11}
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"id":42,"name":"Listen-Name","nrActive":11}
```

## DELETE /list/:id

### Captures:

- *id*: ID der Liste die benutzt werden soll

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[]
```

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[{"id":42,"name":"Listen-Name","nrActive":11}]
```

- Example (`application/json;charset=utf-8`):

```javascript
[{"id":42,"name":"Listen-Name","nrActive":11},{"id":42,"name":"Listen-Name","nrActive":11}]
```

## GET /list/:id

### Captures:

- *id*: ID der Liste die benutzt werden soll

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"id":42,"name":"Listen-Name","nrActive":11}
```

## GET /list/:listId/todos

### Captures:

- *id*: ID der Liste die benutzt werden soll

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[]
```

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[{"id":4711,"listId":42,"text":"Task-Name","finished":false}]
```

- Example (`application/json;charset=utf-8`):

```javascript
[{"id":4711,"listId":42,"text":"Task-Name","finished":false},{"id":4711,"listId":42,"text":"Task-Name","finished":false}]
```

## POST /list/:listId/todos

### Captures:

- *id*: ID der Liste die benutzt werden soll

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
"Text"
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"id":4711,"listId":42,"text":"Task-Name","finished":false}
```

## PUT /list/:listId/todos

### Captures:

- *id*: ID der Liste die benutzt werden soll

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"id":4711,"listId":42,"text":"Task-Name","finished":false}
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"id":4711,"listId":42,"text":"Task-Name","finished":false}
```

## DELETE /list/:listId/todos/:id

### Captures:

- *id*: ID der Liste die benutzt werden soll
- *id*: ID des Tasks der benutzt werden soll

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[]
```

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[{"id":4711,"listId":42,"text":"Task-Name","finished":false}]
```

- Example (`application/json;charset=utf-8`):

```javascript
[{"id":4711,"listId":42,"text":"Task-Name","finished":false},{"id":4711,"listId":42,"text":"Task-Name","finished":false}]
```

## GET /list/:listId/todos/:id

### Captures:

- *id*: ID der Liste die benutzt werden soll
- *id*: ID des Tasks der benutzt werden soll

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"id":4711,"listId":42,"text":"Task-Name","finished":false}
```

## GET /user/login

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
"Text"
```

## POST /user/register

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"name":"your-username","password":"top secret pa$$w0rd"}
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
"Text"
```

