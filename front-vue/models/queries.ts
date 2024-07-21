import * as tanstack from "@tanstack/vue-query";
import type PaginatedResponse from "./domain/page";

type PageParameter = { take: number; skip: number };

type Query<ReturnType> = {
    queryKey: tanstack.QueryKey;
    queryFn: tanstack.QueryFunction<ReturnType, tanstack.QueryKey>;
};

type QueryOptions<ReturnType> = Omit<
    tanstack.QueryOptions<ReturnType, Error, ReturnType, tanstack.QueryKey>,
    "queryFn" | "queryKey" | "initialData"
>;

type PaginatedQuery<ReturnType> = {
    queryKey: tanstack.QueryKey;
    queryFn: (page: PageParameter) => Promise<PaginatedResponse<ReturnType>>;
};

type PaginatedQueryOptions<ReturnType> = Omit<
    tanstack.QueryOptions<
        PaginatedResponse<ReturnType>,
        Error,
        tanstack.InfiniteData<PaginatedResponse<ReturnType>, PageParameter>,
        tanstack.QueryKey,
        PageParameter
    >,
    "queryFn" | "queryKey" | "initialData"
>;

export {
    type PaginatedQuery,
    type PaginatedQueryOptions,
    type PageParameter,
    type Query,
    type QueryOptions,
};
