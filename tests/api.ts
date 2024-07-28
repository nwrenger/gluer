export interface Hello {
    _name: string;
}

export async function post_root(data: Hello): Promise<string | any> {
    const response = await fetch("/", {
        method: "POST",
        headers: {
            "Content-Type": "application/json"
        },
        body: JSON.stringify(data)
    });
    return response.json();
}

