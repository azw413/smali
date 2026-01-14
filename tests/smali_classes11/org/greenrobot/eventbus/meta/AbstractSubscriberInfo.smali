.class public abstract Lorg/greenrobot/eventbus/meta/AbstractSubscriberInfo;
.super Ljava/lang/Object;
.source "AbstractSubscriberInfo.java"

# interfaces
.implements Lorg/greenrobot/eventbus/meta/SubscriberInfo;


# instance fields
.field private final shouldCheckSuperclass:Z

.field private final subscriberClass:Ljava/lang/Class;

.field private final superSubscriberInfoClass:Ljava/lang/Class;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Ljava/lang/Class<",
            "+",
            "Lorg/greenrobot/eventbus/meta/SubscriberInfo;",
            ">;"
        }
    .end annotation
.end field


# direct methods
.method protected constructor <init>(Ljava/lang/Class;Ljava/lang/Class;Z)V
    .locals 0
    .param p1, "subscriberClass"    # Ljava/lang/Class;
    .param p3, "shouldCheckSuperclass"    # Z
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "(",
            "Ljava/lang/Class;",
            "Ljava/lang/Class<",
            "+",
            "Lorg/greenrobot/eventbus/meta/SubscriberInfo;",
            ">;Z)V"
        }
    .end annotation

    .line 31
    .local p2, "superSubscriberInfoClass":Ljava/lang/Class;, "Ljava/lang/Class<+Lorg/greenrobot/eventbus/meta/SubscriberInfo;>;"
    invoke-direct {p0}, Ljava/lang/Object;-><init>()V

    .line 32
    iput-object p1, p0, Lorg/greenrobot/eventbus/meta/AbstractSubscriberInfo;->subscriberClass:Ljava/lang/Class;

    .line 33
    iput-object p2, p0, Lorg/greenrobot/eventbus/meta/AbstractSubscriberInfo;->superSubscriberInfoClass:Ljava/lang/Class;

    .line 34
    iput-boolean p3, p0, Lorg/greenrobot/eventbus/meta/AbstractSubscriberInfo;->shouldCheckSuperclass:Z

    .line 35
    return-void
.end method


# virtual methods
.method protected createSubscriberMethod(Ljava/lang/String;Ljava/lang/Class;)Lorg/greenrobot/eventbus/SubscriberMethod;
    .locals 6
    .param p1, "methodName"    # Ljava/lang/String;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "(",
            "Ljava/lang/String;",
            "Ljava/lang/Class<",
            "*>;)",
            "Lorg/greenrobot/eventbus/SubscriberMethod;"
        }
    .end annotation

    .line 62
    .local p2, "eventType":Ljava/lang/Class;, "Ljava/lang/Class<*>;"
    sget-object v3, Lorg/greenrobot/eventbus/ThreadMode;->POSTING:Lorg/greenrobot/eventbus/ThreadMode;

    const/4 v4, 0x0

    const/4 v5, 0x0

    move-object v0, p0

    move-object v1, p1

    move-object v2, p2

    invoke-virtual/range {v0 .. v5}, Lorg/greenrobot/eventbus/meta/AbstractSubscriberInfo;->createSubscriberMethod(Ljava/lang/String;Ljava/lang/Class;Lorg/greenrobot/eventbus/ThreadMode;IZ)Lorg/greenrobot/eventbus/SubscriberMethod;

    move-result-object v0

    return-object v0
.end method

.method protected createSubscriberMethod(Ljava/lang/String;Ljava/lang/Class;Lorg/greenrobot/eventbus/ThreadMode;)Lorg/greenrobot/eventbus/SubscriberMethod;
    .locals 6
    .param p1, "methodName"    # Ljava/lang/String;
    .param p3, "threadMode"    # Lorg/greenrobot/eventbus/ThreadMode;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "(",
            "Ljava/lang/String;",
            "Ljava/lang/Class<",
            "*>;",
            "Lorg/greenrobot/eventbus/ThreadMode;",
            ")",
            "Lorg/greenrobot/eventbus/SubscriberMethod;"
        }
    .end annotation

    .line 66
    .local p2, "eventType":Ljava/lang/Class;, "Ljava/lang/Class<*>;"
    const/4 v4, 0x0

    const/4 v5, 0x0

    move-object v0, p0

    move-object v1, p1

    move-object v2, p2

    move-object v3, p3

    invoke-virtual/range {v0 .. v5}, Lorg/greenrobot/eventbus/meta/AbstractSubscriberInfo;->createSubscriberMethod(Ljava/lang/String;Ljava/lang/Class;Lorg/greenrobot/eventbus/ThreadMode;IZ)Lorg/greenrobot/eventbus/SubscriberMethod;

    move-result-object v0

    return-object v0
.end method

.method protected createSubscriberMethod(Ljava/lang/String;Ljava/lang/Class;Lorg/greenrobot/eventbus/ThreadMode;IZ)Lorg/greenrobot/eventbus/SubscriberMethod;
    .locals 9
    .param p1, "methodName"    # Ljava/lang/String;
    .param p3, "threadMode"    # Lorg/greenrobot/eventbus/ThreadMode;
    .param p4, "priority"    # I
    .param p5, "sticky"    # Z
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "(",
            "Ljava/lang/String;",
            "Ljava/lang/Class<",
            "*>;",
            "Lorg/greenrobot/eventbus/ThreadMode;",
            "IZ)",
            "Lorg/greenrobot/eventbus/SubscriberMethod;"
        }
    .end annotation

    .line 72
    .local p2, "eventType":Ljava/lang/Class;, "Ljava/lang/Class<*>;"
    :try_start_0
    iget-object v0, p0, Lorg/greenrobot/eventbus/meta/AbstractSubscriberInfo;->subscriberClass:Ljava/lang/Class;

    const/4 v1, 0x1

    new-array v1, v1, [Ljava/lang/Class;

    const/4 v2, 0x0

    aput-object p2, v1, v2

    invoke-virtual {v0, p1, v1}, Ljava/lang/Class;->getDeclaredMethod(Ljava/lang/String;[Ljava/lang/Class;)Ljava/lang/reflect/Method;

    move-result-object v4

    .line 73
    .local v4, "method":Ljava/lang/reflect/Method;
    new-instance v0, Lorg/greenrobot/eventbus/SubscriberMethod;

    move-object v3, v0

    move-object v5, p2

    move-object v6, p3

    move v7, p4

    move v8, p5

    invoke-direct/range {v3 .. v8}, Lorg/greenrobot/eventbus/SubscriberMethod;-><init>(Ljava/lang/reflect/Method;Ljava/lang/Class;Lorg/greenrobot/eventbus/ThreadMode;IZ)V
    :try_end_0
    .catch Ljava/lang/NoSuchMethodException; {:try_start_0 .. :try_end_0} :catch_0

    return-object v0

    .line 74
    .end local v4    # "method":Ljava/lang/reflect/Method;
    :catch_0
    move-exception v0

    .line 75
    .local v0, "e":Ljava/lang/NoSuchMethodException;
    new-instance v1, Lorg/greenrobot/eventbus/EventBusException;

    new-instance v2, Ljava/lang/StringBuilder;

    invoke-direct {v2}, Ljava/lang/StringBuilder;-><init>()V

    const-string v3, "Could not find subscriber method in "

    invoke-virtual {v2, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    iget-object v3, p0, Lorg/greenrobot/eventbus/meta/AbstractSubscriberInfo;->subscriberClass:Ljava/lang/Class;

    invoke-virtual {v2, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/Object;)Ljava/lang/StringBuilder;

    move-result-object v2

    const-string v3, ". Maybe a missing ProGuard rule?"

    invoke-virtual {v2, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    invoke-virtual {v2}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v2

    invoke-direct {v1, v2, v0}, Lorg/greenrobot/eventbus/EventBusException;-><init>(Ljava/lang/String;Ljava/lang/Throwable;)V

    throw v1
.end method

.method public getSubscriberClass()Ljava/lang/Class;
    .locals 1

    .line 39
    iget-object v0, p0, Lorg/greenrobot/eventbus/meta/AbstractSubscriberInfo;->subscriberClass:Ljava/lang/Class;

    return-object v0
.end method

.method public getSuperSubscriberInfo()Lorg/greenrobot/eventbus/meta/SubscriberInfo;
    .locals 2

    .line 44
    iget-object v0, p0, Lorg/greenrobot/eventbus/meta/AbstractSubscriberInfo;->superSubscriberInfoClass:Ljava/lang/Class;

    if-nez v0, :cond_0

    .line 45
    const/4 v0, 0x0

    return-object v0

    .line 48
    :cond_0
    :try_start_0
    invoke-virtual {v0}, Ljava/lang/Class;->newInstance()Ljava/lang/Object;

    move-result-object v0

    check-cast v0, Lorg/greenrobot/eventbus/meta/SubscriberInfo;
    :try_end_0
    .catch Ljava/lang/InstantiationException; {:try_start_0 .. :try_end_0} :catch_1
    .catch Ljava/lang/IllegalAccessException; {:try_start_0 .. :try_end_0} :catch_0

    return-object v0

    .line 51
    :catch_0
    move-exception v0

    .line 52
    .local v0, "e":Ljava/lang/IllegalAccessException;
    new-instance v1, Ljava/lang/RuntimeException;

    invoke-direct {v1, v0}, Ljava/lang/RuntimeException;-><init>(Ljava/lang/Throwable;)V

    throw v1

    .line 49
    .end local v0    # "e":Ljava/lang/IllegalAccessException;
    :catch_1
    move-exception v0

    .line 50
    .local v0, "e":Ljava/lang/InstantiationException;
    new-instance v1, Ljava/lang/RuntimeException;

    invoke-direct {v1, v0}, Ljava/lang/RuntimeException;-><init>(Ljava/lang/Throwable;)V

    throw v1
.end method

.method public shouldCheckSuperclass()Z
    .locals 1

    .line 58
    iget-boolean v0, p0, Lorg/greenrobot/eventbus/meta/AbstractSubscriberInfo;->shouldCheckSuperclass:Z

    return v0
.end method
