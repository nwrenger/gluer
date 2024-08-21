const PREFIX = '';

namespace api {
    /**
        Might want to look into the `api.ts` file to see the docstring for this struct
    */
    export interface Age {
        /**
            Even supports docstring on fields and optional fields
        */
        age?: AgeInner;
    }

    export interface AgeInner {
        /**
            This gets converted to a `string` on the TypeScript side
            because `numbers` there cannot be greater than 64 bits
        */
        age: string;
    }

    export interface Hello<T, S> {
        name: S;
        vec: T[];
    }

    export interface Huh<T> {
        huh: T;
    }

    export interface QueryOptions {
        id: number;
        query: string;
    }

    export enum Alphabet {
        A = "A",
        B = "B",
        C = "C",
    }

    /**
        An example how an api error type could look like
    */
    export enum Error {
        /**
            Normal 404 error
        */
        NotFound = "NotFound",
        /**
            Internally something really bad happened
        */
        InternalServerError = "InternalServerError",
    }

    export type Result<T> = T | Error;

    export type S = string;

    async function fetch_api(endpoint: string, options: RequestInit): Promise<any> {
        const response = await fetch(endpoint, {
            headers: {
                "Content-Type": "application/json",
                ...options.headers,
            },
            ...options,
        });
        if (response.headers.get('Content-Length') === '0') {
			return;
		} else {
			return response.json();
		}
    }

    function query_str(params: Record<string, any>): string {
		if (params) {
			let data: Record<string, string> = {};
			for (let key in params) {
				if (params[key] != null) data[key] = params[key].toString();
			}
			return '?' + new URLSearchParams(data).toString();
		}
		return '';
	}

    /**
        Docstrings for functions are also supported
    */
    export async function add_root(path: number, data: Result<Hello<Hello<Huh<Huh<Hello<Age, string>>>, string>, string>>): Promise<Result<string>> {
        return fetch_api(`${PREFIX}/${encodeURIComponent(path)}`, {
            method: "POST", 
            body: JSON.stringify(data)
        });
    }

    export async function fetch_other(query: QueryOptions): Promise<string> {
        return fetch_api(`${PREFIX}/other${query_str(query)}`, {
            method: "GET", 
        });
    }

    export async function fetch_root(queryMap: Record<string, string>, path: number): Promise<string> {
        return fetch_api(`${PREFIX}/${encodeURIComponent(path)}?${new URLSearchParams(queryMap).toString()}`, {
            method: "GET", 
        });
    }

    export async function get_alphabet(pathTuple: [Alphabet, S]): Promise<[Alphabet, S]> {
        return fetch_api(`${PREFIX}/char/${encodeURIComponent(pathTuple[0])}/metadata/${encodeURIComponent(pathTuple[1])}`, {
            method: "GET", 
        });
    }
}

export default api;
