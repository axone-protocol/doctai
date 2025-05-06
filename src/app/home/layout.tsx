// app/home/layout.tsx
import AuthGuard from "@/components/AuthGuard/AuthGuard";
import SidebarLayout from "@/components/SidebarLayout/SidebarLayout";
import PreloadZoneWrapper from "../../components/PreloadZoneWrapper/PreloadZoneWrapper";

export default function HomeLayout({ children }: { children: React.ReactNode }) {
    return (
        <AuthGuard>
            <PreloadZoneWrapper>
                <SidebarLayout>{children}</SidebarLayout>
            </PreloadZoneWrapper>
        </AuthGuard>
    );
}