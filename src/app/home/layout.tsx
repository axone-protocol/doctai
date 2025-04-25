// app/home/layout.tsx
import AuthGuard from "@/components/AuthGuard/AuthGuard";
import SidebarLayout from "@/components/SidebarLayout/SidebarLayout";

export default function HomeLayout({
    children,
}: {
    children: React.ReactNode;
}) {
    return (
        <AuthGuard>
            <SidebarLayout>{children}</SidebarLayout>
        </AuthGuard>
    );
}
