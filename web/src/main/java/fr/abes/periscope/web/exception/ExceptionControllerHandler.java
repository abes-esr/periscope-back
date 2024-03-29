package fr.abes.periscope.web.exception;

import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.exc.InvalidTypeIdException;
import com.fasterxml.jackson.databind.exc.MismatchedInputException;
import fr.abes.periscope.core.exception.IllegalCriterionException;
import fr.abes.periscope.core.exception.IllegalPpnException;
import lombok.extern.slf4j.Slf4j;
import org.apache.solr.client.solrj.impl.BaseHttpSolrClient;
import org.apache.solr.client.solrj.impl.HttpSolrClient;
import org.modelmapper.MappingException;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.MissingServletRequestParameterException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.context.request.WebRequest;
import org.springframework.web.servlet.NoHandlerFoundException;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;

import javax.validation.ConstraintViolation;
import javax.validation.ConstraintViolationException;
import java.util.List;
import java.util.Set;

/**
 * Gestionnaire des exceptions de l'API.
 * Cette classe récupère toutes les exceptions et renvoi un message d'erreur
 */
@Slf4j
@Order(Ordered.HIGHEST_PRECEDENCE)
@ControllerAdvice
public class ExceptionControllerHandler extends ResponseEntityExceptionHandler {

    private ResponseEntity<Object> buildResponseEntity(ApiReturnError apiReturnError) {
        return new ResponseEntity<>(apiReturnError, apiReturnError.getStatus());
    }

    /**
     * Erreur de lecture / décodage des paramètres d'une requête HTTP
     *
     * @param ex
     * @param headers
     * @param status
     * @param request
     * @return
     */
    @Override
    protected ResponseEntity<Object> handleHttpMessageNotReadable(HttpMessageNotReadableException ex, HttpHeaders headers, HttpStatus status, WebRequest request) {

        String error = "Malformed JSON request";

        if (ex.getCause() instanceof MismatchedInputException && !(ex.getCause() instanceof InvalidTypeIdException)) {
            String targetType = ((MismatchedInputException) ex.getCause()).getTargetType().getSimpleName();

            List<JsonMappingException.Reference> errors = ((MismatchedInputException) ex.getCause()).getPath();
            String property = errors.get(errors.size() - 1).getFieldName();

            log.error(ex.getLocalizedMessage());
            return buildResponseEntity(new ApiReturnError(HttpStatus.BAD_REQUEST, error, new MismatchedJsonTypeException(property + " need to be type of '" + targetType + "'")));
        }

        log.error(ex.getLocalizedMessage());
        return buildResponseEntity(new ApiReturnError(HttpStatus.BAD_REQUEST, error, ex));
    }

    /**
     * Vérifier les méthodes correspondent avec les URI dans le controller
     *
     * @param ex
     * @param headers
     * @param status
     * @param request
     * @return
     */
    @Override
    protected ResponseEntity<Object> handleHttpRequestMethodNotSupported(HttpRequestMethodNotSupportedException ex, HttpHeaders headers, HttpStatus status, WebRequest request) {
        String error = "Method is not supported for this request";
        log.error(ex.getLocalizedMessage());
        return buildResponseEntity(new ApiReturnError(HttpStatus.METHOD_NOT_ALLOWED, error, ex));
    }

    /**
     * Vérifier la validité (@Valid) des paramètres de la requête
     *
     * @param ex
     * @param headers
     * @param status
     * @param request
     * @return
     */
    @Override
    protected ResponseEntity<Object> handleMethodArgumentNotValid(MethodArgumentNotValidException ex, HttpHeaders headers, HttpStatus status, WebRequest request) {
        String error = "The credentials are not valid";
        BindingResult result = ex.getBindingResult();
        List<FieldError> fieldErrors = result.getFieldErrors();
        String msg = "";
        for (FieldError fieldError : fieldErrors) {
            msg += fieldError.getDefaultMessage() + " ";
        }
        log.error(msg);
        return buildResponseEntity(new ApiReturnError(HttpStatus.BAD_REQUEST, error, new IllegalCriterionException(msg)));
    }

    /**
     * Page 404
     *
     * @param ex
     * @param headers
     * @param status
     * @param request
     * @return
     */
    @Override
    protected ResponseEntity<Object> handleNoHandlerFoundException(NoHandlerFoundException ex, HttpHeaders headers, HttpStatus status, WebRequest request) {
        String error = "Page not found";
        log.error(ex.getLocalizedMessage());
        return buildResponseEntity(new ApiReturnError(HttpStatus.NOT_FOUND, error, ex));
    }

    /**
     * Erreur de paramètre
     *
     * @param ex
     * @param headers
     * @param status
     * @param request
     * @return
     */
    @Override
    protected ResponseEntity<Object> handleMissingServletRequestParameter(MissingServletRequestParameterException ex, HttpHeaders headers, HttpStatus status, WebRequest request) {
        String error = "Missing request parameter";
        log.error(ex.getLocalizedMessage());
        return buildResponseEntity(new ApiReturnError(HttpStatus.BAD_REQUEST, error, ex));
    }

    @ExceptionHandler(ConstraintViolationException.class)
    public ResponseEntity handle(ConstraintViolationException ex) {
        Set<ConstraintViolation<?>> violations = ex.getConstraintViolations();
        String errorMessage;
        if (!violations.isEmpty()) {
            StringBuilder builder = new StringBuilder();
            violations.forEach(violation -> builder.append(" " + violation.getMessage()));
            errorMessage = builder.toString();
        } else {
            errorMessage = "ConstraintViolationException occured.";
        }
        return buildResponseEntity(new ApiReturnError(HttpStatus.BAD_REQUEST, errorMessage, ex));
    }

    /**
     * Si la transformation DTO a échoué
     *
     * @param ex MappingException
     * @return
     */
    @ExceptionHandler(MappingException.class)
    protected ResponseEntity<Object> handleMappingException(MappingException ex) {
        String error = "Malformed JSON request";
        log.error(ex.getCause().getLocalizedMessage());
        return buildResponseEntity(new ApiReturnError(HttpStatus.BAD_REQUEST, error, ex.getCause()));
    }

    /**
     * Si le critère de recherche est malformé
     *
     * @param ex IllegalCriterionException
     * @return
     */
    @ExceptionHandler(IllegalCriterionException.class)
    protected ResponseEntity<Object> handleIllegalCriterionException(IllegalCriterionException ex) {
        String error = "Malformed JSON request";
        log.error(ex.getLocalizedMessage());
        return buildResponseEntity(new ApiReturnError(HttpStatus.BAD_REQUEST, error, ex));
    }

    /**
     * Si la connexion / requête avec le SolR a echoué, on loggue l'exception
     * et on renvoit une erreur standard à l'API pour masquer l'URL du serveur SolR
     *
     * @param ex
     * @return
     */
    @ExceptionHandler(BaseHttpSolrClient.RemoteSolrException.class)
    protected ResponseEntity<Object> handleRemoteSolrException(BaseHttpSolrClient.RemoteSolrException ex) {
        String error = "SolR server error";
        log.error(ex.getLocalizedMessage());
        return buildResponseEntity(new ApiReturnError(HttpStatus.INTERNAL_SERVER_ERROR, error, new Exception("Something was wrong with the database server")));
    }

    /**
     * Si un mauvais argument est passé en paramètre d'une requête
     * @param ex
     * @return
     */
    @ExceptionHandler(IllegalArgumentException.class)
    protected ResponseEntity<Object> handleIllegalArgumentException(IllegalArgumentException ex) {
        String error = "Argument incorrect";
        log.error("Argument incorrect : " + ex.getLocalizedMessage());
        return buildResponseEntity(new ApiReturnError(HttpStatus.BAD_REQUEST, error, ex));
    }

    @ExceptionHandler(IllegalPpnException.class)
    protected ResponseEntity<Object> handleIllegalPpnException(IllegalPpnException ex) {
        String error = "Invalid PPN";
        log.error(ex.getLocalizedMessage());
        return buildResponseEntity(new ApiReturnError(HttpStatus.BAD_REQUEST, error, ex));
    }
}