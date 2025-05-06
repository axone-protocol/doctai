import { create } from "zustand";

interface AxoneStore {
  zoneDID: string;
  minioDID: string;
  zoneGovAddress: string;
  minioGovAddress: string;
  zoneGovCode: string;
  minioGovCode: string;
  setAll: (info: Partial<AxoneStore>) => void;
}

export const useAxoneStore = create<AxoneStore>((set) => ({
  zoneDID: "",
  minioDID: "",
  zoneGovAddress: "",
  minioGovAddress: "",
  zoneGovCode: "",
  minioGovCode: "",
  setAll: (info) => set(info),
}));

export function setAxoneStoreFromZoneInfo(info: Partial<AxoneStore>) {
  useAxoneStore.getState().setAll(info);
}
