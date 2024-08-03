const BASE = '';

namespace api {
    export interface Age {
        age: string;
    }

    export interface Hello<T, S> {
        name: S;
        vec: T[];
    }

    export interface Huh<T> {
        huh: T;
    }

    export type Enum = "A" | "B" | "C";

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

    export async function add_root(path: number, data: Hello<Hello<Huh<Huh<Hello<Age, string>>>, string>, string>): Promise<string> {
        return fetch_api(`${BASE}/${encodeURIComponent(path)}`, {
            method: "POST", 
            body: JSON.stringify(data)
        });
    }

    export async function fetch_root(queryMap: Record<string, string>, path: number): Promise<string> {
        return fetch_api(`${BASE}/${encodeURIComponent(path)}?${new URLSearchParams(queryMap).toString()}`, {
            method: "GET", 
        });
    }

    export async function get_enum(): Promise<Enum> {
        return fetch_api(`${BASE}/`, {
            method: "GET", 
        });
    }
}

export default api;
