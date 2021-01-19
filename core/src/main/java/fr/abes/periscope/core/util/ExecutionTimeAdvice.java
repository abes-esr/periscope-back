package fr.abes.periscope.core.util;

import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.boot.autoconfigure.condition.ConditionalOnExpression;
import org.springframework.stereotype.Component;

@Aspect
@Component
@Slf4j
@ConditionalOnExpression("${aspect.enabled:true}")
public class ExecutionTimeAdvice {

    @Around("@annotation(fr.abes.periscope.core.util.TrackExecutionTime)")
    public Object executionTime(ProceedingJoinPoint point) throws Throwable {
        long startTime = System.currentTimeMillis();
        Object object = point.proceed();
        long endtime = System.currentTimeMillis();
        log.debug("Timing ["+ point.getSignature().getDeclaringTypeName() +"] Method '"+ point.getSignature().getName() + "' takes  " + (endtime-startTime) +" ms");
        return object;
    }

}
