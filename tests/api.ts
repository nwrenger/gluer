const BASE = '';

namespace api {
    /**
        Might want to look into the `api.ts` file to see the docstring for this struct
    */
    export interface Age {
        age: AgeInner;
    }

    export interface AgeInner {
        age: number;
    }

    export interface Hello<T, S> {
        name: S;
        vec: T[];
    }

    export interface Huh<T> {
        huh: T;
    }

    export type Alphabet = "A" | "B" | "C";

    export type Error = "NotFound" | "InternalServerError";

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
        return response.json();
    }

    function query_str(params: Record<string, any>): string {
		if (params) {
			let data: Record<string, string> = {};
			for (let key in params) {
				if (params[key] != null) data[key] = params[key].toString();
			}
			// the URLSearchParams escapes any problematic values
			return '?' + new URLSearchParams(data).toString();
		}
		return '';
	}

    export async function add_root(path: number, data: Result<Hello<Hello<Huh<Huh<Hello<Age, string>>>, string>, string>>): Promise<Result<string>> {
        return fetch_api(`${BASE}/${encodeURIComponent(path)}`, {
            method: "POST", 
            body: JSON.stringify(data)
        });
    }

    /**
        An example of a simple function with a `Path` and `Query` extractor
    */
    export async function fetch_root(queryMap: Record<string, string>, path: number): Promise<string> {
        return fetch_api(`${BASE}/${encodeURIComponent(path)}?${new URLSearchParams(queryMap).toString()}`, {
            method: "GET", 
        });
    }

    export async function get_alphabet(pathTuple: [Alphabet, S]): Promise<[Alphabet, S]> {
        return fetch_api(`${BASE}/char/${encodeURIComponent(pathTuple[0])}/metadata/${encodeURIComponent(pathTuple[1])}`, {
            method: "GET", 
        });
    }
}

export default api;
