<script setup lang="ts">
import { API } from "~/api/api";
const scannerStatusQuery = API.getScannerStatus();
const scannerStatus = useQuery(scannerStatusQuery);
const scan = () => API.scanNewFiles().then(() => scannerStatus.refetch());
const clean = () => API.cleanFiles().then(() => scannerStatus.refetch());
</script>
<template>
    <div class="max-w-screen-lg w-full h-full p-3">
        <h1 class="prose-xl">Settings</h1>
        <div class="divider" />
        <h2 class="prose-lg pb-2">Scanner</h2>
        <div class="w-full flex justify-between items-center">
            <p>Status</p>
            <p v-if="scannerStatus.data">
                {{ scannerStatus.data?.value?.status }}
                <button
                    class="btn btn-xs btn-circle"
                    @click="scannerStatus.refetch()"
                >
                    <fa icon="arrows-rotate" />
                </button>
            </p>
            <div v-else class="skeleton h-4 w-10" />
        </div>
        <div class="w-full flex justify-evenly items-center">
            <button class="btn btn-sm font-medium btn-primary" @click="scan()">
                Scan New Files
            </button>
            <button class="btn btn-sm font-medium btn-secondary" @click="clean()">
                Clean Library
            </button>
        </div>

        <div class="divider" />
    </div>
</template>
