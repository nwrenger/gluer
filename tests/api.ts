export interface Hello {
    name: string;
}

export async function add_root(path: number, data: Hello): Promise<Hello[]> {
    const response = await fetch(`/${encodeURIComponent(path)}`, {
        method: "POST",
        headers: {
            "Content-Type": "application/json"
        },
        body: JSON.stringify(data)
    });
    return response.json();
}

export async function fetch_root(queryMap: Record<string, string>, path: number): Promise<string> {
    const response = await fetch(`/${encodeURIComponent(path)}?${new URLSearchParams(queryMap).toString()}`, {
        method: "GET",
        headers: {
            "Content-Type": "application/json"
        },
        body: undefined
    });
    return response.json();
}

