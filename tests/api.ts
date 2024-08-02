namespace api {
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

    async function fetchApi(endpoint: string, options: RequestInit): Promise<any> {
        const response = await fetch(endpoint, {
            headers: {
                "Content-Type": "application/json",
                ...options.headers,
            },
            ...options,
        });
        return response.json();
    }

    export async function add_root(path: number, data: Hello<Hello<Huh<Age>, string>, string>): Promise<string> {
        return fetchApi(`/${encodeURIComponent(path)}`, {
            method: "POST", 
            body: JSON.stringify(data)
        });
    }

    export async function fetch_root(queryMap: Record<string, string>, path: number): Promise<string> {
        return fetchApi(`/${encodeURIComponent(path)}?${new URLSearchParams(queryMap).toString()}`, {
            method: "GET", 
        });
    }
}

export default api;
