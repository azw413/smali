.class public Lorg/greenrobot/eventbus/android/AndroidDependenciesDetector;
.super Ljava/lang/Object;
.source "AndroidDependenciesDetector.java"


# static fields
.field private static final ANDROID_COMPONENTS_IMPLEMENTATION_CLASS_NAME:Ljava/lang/String; = "org.greenrobot.eventbus.android.AndroidComponentsImpl"


# direct methods
.method public constructor <init>()V
    .locals 0

    .line 7
    invoke-direct {p0}, Ljava/lang/Object;-><init>()V

    return-void
.end method

.method public static areAndroidComponentsAvailable()Z
    .locals 2

    .line 30
    :try_start_0
    const-string v0, "org.greenrobot.eventbus.android.AndroidComponentsImpl"

    invoke-static {v0}, Ljava/lang/Class;->forName(Ljava/lang/String;)Ljava/lang/Class;
    :try_end_0
    .catch Ljava/lang/ClassNotFoundException; {:try_start_0 .. :try_end_0} :catch_0

    .line 31
    const/4 v0, 0x1

    return v0

    .line 33
    :catch_0
    move-exception v0

    .line 34
    .local v0, "ex":Ljava/lang/ClassNotFoundException;
    const/4 v1, 0x0

    return v1
.end method

.method public static instantiateAndroidComponents()Lorg/greenrobot/eventbus/android/AndroidComponents;
    .locals 3

    .line 41
    :try_start_0
    const-string v0, "org.greenrobot.eventbus.android.AndroidComponentsImpl"

    invoke-static {v0}, Ljava/lang/Class;->forName(Ljava/lang/String;)Ljava/lang/Class;

    move-result-object v0

    .line 42
    .local v0, "impl":Ljava/lang/Class;, "Ljava/lang/Class<*>;"
    const/4 v1, 0x0

    new-array v2, v1, [Ljava/lang/Class;

    invoke-virtual {v0, v2}, Ljava/lang/Class;->getConstructor([Ljava/lang/Class;)Ljava/lang/reflect/Constructor;

    move-result-object v2

    new-array v1, v1, [Ljava/lang/Object;

    invoke-virtual {v2, v1}, Ljava/lang/reflect/Constructor;->newInstance([Ljava/lang/Object;)Ljava/lang/Object;

    move-result-object v1

    check-cast v1, Lorg/greenrobot/eventbus/android/AndroidComponents;
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    return-object v1

    .line 44
    .end local v0    # "impl":Ljava/lang/Class;, "Ljava/lang/Class<*>;"
    :catchall_0
    move-exception v0

    .line 45
    .local v0, "ex":Ljava/lang/Throwable;
    const/4 v1, 0x0

    return-object v1
.end method

.method public static isAndroidSDKAvailable()Z
    .locals 5

    .line 12
    const/4 v0, 0x0

    :try_start_0
    const-string v1, "android.os.Looper"

    invoke-static {v1}, Ljava/lang/Class;->forName(Ljava/lang/String;)Ljava/lang/Class;

    move-result-object v1

    .line 13
    .local v1, "looperClass":Ljava/lang/Class;, "Ljava/lang/Class<*>;"
    const-string v2, "getMainLooper"

    new-array v3, v0, [Ljava/lang/Class;

    invoke-virtual {v1, v2, v3}, Ljava/lang/Class;->getDeclaredMethod(Ljava/lang/String;[Ljava/lang/Class;)Ljava/lang/reflect/Method;

    move-result-object v2

    .line 14
    .local v2, "getMainLooper":Ljava/lang/reflect/Method;
    new-array v3, v0, [Ljava/lang/Object;

    const/4 v4, 0x0

    invoke-virtual {v2, v4, v3}, Ljava/lang/reflect/Method;->invoke(Ljava/lang/Object;[Ljava/lang/Object;)Ljava/lang/Object;

    move-result-object v3
    :try_end_0
    .catch Ljava/lang/ClassNotFoundException; {:try_start_0 .. :try_end_0} :catch_3
    .catch Ljava/lang/NoSuchMethodException; {:try_start_0 .. :try_end_0} :catch_2
    .catch Ljava/lang/IllegalAccessException; {:try_start_0 .. :try_end_0} :catch_1
    .catch Ljava/lang/reflect/InvocationTargetException; {:try_start_0 .. :try_end_0} :catch_0

    .line 15
    .local v3, "mainLooper":Ljava/lang/Object;
    if-eqz v3, :cond_0

    const/4 v0, 0x1

    :cond_0
    return v0

    .line 20
    .end local v1    # "looperClass":Ljava/lang/Class;, "Ljava/lang/Class<*>;"
    .end local v2    # "getMainLooper":Ljava/lang/reflect/Method;
    .end local v3    # "mainLooper":Ljava/lang/Object;
    :catch_0
    move-exception v1

    goto :goto_0

    .line 19
    :catch_1
    move-exception v1

    goto :goto_0

    .line 18
    :catch_2
    move-exception v1

    goto :goto_0

    .line 17
    :catch_3
    move-exception v1

    .line 20
    :goto_0
    nop

    .line 22
    return v0
.end method
